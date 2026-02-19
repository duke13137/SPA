(ns iam-oracle
  (:require
   [cheshire.core :as json]))

(require '[babashka.pods :as pods])
(pods/load-pod 'huahaiy/datalevin "0.10.5")
(require '[pod.huahaiy.datalevin :as d])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {;; ── CI envelope ──────────────────────────────────────────────────────
   :ci/arn              {:db/unique    :db.unique/identity
                         :db/valueType :db.type/string}
   :ci/account-id       {:db/valueType :db.type/string}
   :ci/region           {:db/valueType :db.type/string}
   :ci/resource-type    {:db/valueType :db.type/string}
   :ci/resource-id      {:db/valueType :db.type/string}
   :ci/resource-name    {:db/valueType :db.type/string}
   :ci/status           {:db/valueType :db.type/string}
   :ci/capture-time     {:db/valueType :db.type/string}
   :ci/config-state-id  {:db/valueType :db.type/string}

   ;; ── IAM Role fields (on CI entity) ───────────────────────────────────
   :role/id               {:db/valueType :db.type/string}
   :role/path             {:db/valueType :db.type/string}
   :role/create-date      {:db/valueType :db.type/string}
   :role/max-session      {:db/valueType :db.type/long}
   :role/last-used-date   {:db/valueType :db.type/string}
   :role/last-used-region {:db/valueType :db.type/string}
   :role/boundary-arn     {:db/valueType :db.type/string}
   :role/trust-policy     {:db/valueType :db.type/ref}
   :role/attached         {:db/valueType   :db.type/ref
                           :db/cardinality :db.cardinality/many}
   :role/inline           {:db/valueType   :db.type/ref
                           :db/cardinality :db.cardinality/many}
   :role/trusted-by        {:db/valueType   :db.type/ref
                            :db/cardinality :db.cardinality/many}

   ;; ── IAM Policy entity ─────────────────────────────────────────────────
   ;; :policy/arn  - set for managed policies (upsert key)
   ;; :policy/id   - synthetic UUID for inline/trust policies (upsert key)
   :policy/arn        {:db/unique    :db.unique/identity
                       :db/valueType :db.type/string}
   :policy/id         {:db/unique    :db.unique/identity
                       :db/valueType :db.type/string}
   :policy/name       {:db/valueType :db.type/string}
   :policy/type       {:db/valueType :db.type/string}   ; "managed" | "inline" | "trust"
   :policy/version    {:db/valueType :db.type/string}
   :policy/statements {:db/valueType   :db.type/ref
                       :db/cardinality :db.cardinality/many}

   ;; ── Policy Statement entity ────────────────────────────────────────────
   ;; :stmt/id is a synthetic UUID — unique identity for lookup refs
   :stmt/id             {:db/unique    :db.unique/identity
                         :db/valueType :db.type/string}
   :stmt/sid            {:db/valueType :db.type/string}
   :stmt/effect         {:db/valueType :db.type/string}   ; "Allow" | "Deny"
   :stmt/actions        {:db/valueType   :db.type/string
                         :db/cardinality :db.cardinality/many}
   :stmt/resources      {:db/valueType   :db.type/string
                         :db/cardinality :db.cardinality/many}
   :stmt/principal-type {:db/valueType :db.type/string}   ; "Service" | "AWS" | "Federated"
   :stmt/principals     {:db/valueType   :db.type/string
                         :db/cardinality :db.cardinality/many}
   :stmt/condition      {:db/valueType :db.type/string}}) ; serialized JSON

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def db-path "/tmp/bb-iam-server")

(defonce conn (d/get-conn db-path schema))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-json-field [v]
  (cond (map? v) v (string? v) (json/parse-string v true) :else nil))

(defn- ensure-vec [x]
  (cond (nil? x) [] (sequential? x) (vec x) :else [x]))

(defn- uuid-str [] (str (random-uuid)))

(defn- role-arn? [s]
  (and (string? s) (re-matches #"arn:aws:iam::\d+:role/.+" s)))

(defn- extract-trusted-role-arns
  "Extract role ARNs from a trust policy document's Allow-statement principals."
  [trust-policy-doc]
  (let [doc   (parse-json-field trust-policy-doc)
        stmts (ensure-vec (:Statement doc))]
    (->> stmts
         (filter #(= "Allow" (:Effect %)))
         (mapcat (fn [stmt]
                   (let [p (:Principal stmt)]
                     (cond
                       (map? p)    (mapcat #(ensure-vec %) (vals p))
                       (string? p) [p]
                       :else       []))))
         (filter role-arn?)
         distinct
         vec)))

(defn- statements->txdata
  "Convert parsed Statement list to tx-data.
   Returns [lookup-refs tx-maps] where lookup-refs are [:stmt/id uuid] vectors."
  [statements]
  (reduce
   (fn [[refs txs] stmt]
     (let [sid         (uuid-str)
           principal   (:Principal stmt)
           prin-type   (cond (map? principal)    (-> principal keys first name)
                             (string? principal) "AWS"
                             :else               nil)
           prin-vals   (cond (map? principal)    (mapcat #(if (string? %) [%] (vec %)) (vals principal))
                             (string? principal) [principal]
                             :else               [])
           actions     (ensure-vec (:Action stmt))
           resources   (ensure-vec (or (:Resource stmt) (:NotResource stmt)))
           tx          (cond-> {:stmt/id     sid
                                :stmt/effect (:Effect stmt)
                                :stmt/actions   actions
                                :stmt/resources resources}
                         (:Sid stmt)       (assoc :stmt/sid (:Sid stmt))
                         (seq prin-vals)   (assoc :stmt/principals prin-vals
                                                  :stmt/principal-type prin-type)
                         (:Condition stmt) (assoc :stmt/condition
                                                  (json/generate-string (:Condition stmt))))]
       [(conj refs [:stmt/id sid]) (conj txs tx)]))
   [[] []]
   statements))

(defn- policy-doc->txdata
  "Build tx-data for a policy document.
   Returns [lookup-ref all-tx-maps] where lookup-ref is [:policy/id uuid] or [:policy/arn arn]."
  [policy-doc policy-name policy-type & [managed-arn]]
  (let [doc            (parse-json-field policy-doc)
        stmts          (ensure-vec (:Statement doc))
        [srefs stmts-tx] (statements->txdata stmts)
        pid            (uuid-str)
        policy-tx      (cond-> {:policy/id      pid
                                :policy/name    policy-name
                                :policy/type    policy-type
                                :policy/version (get doc :Version "2012-10-17")
                                :policy/statements srefs}
                         managed-arn (assoc :policy/arn managed-arn))
        lookup-ref     (if managed-arn [:policy/arn managed-arn] [:policy/id pid])]
    [lookup-ref (conj stmts-tx policy-tx)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CI → tx-data
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- role-txdata [ci cfg]
  (let [[trust-ref trust-txs]
        (policy-doc->txdata (:assumeRolePolicyDocument cfg) "trust" "trust")

        trusted-role-arns
        (extract-trusted-role-arns (:assumeRolePolicyDocument cfg))

        trusted-refs
        (mapv #(vector :ci/arn %) trusted-role-arns)

        inline-pairs
        (mapv #(policy-doc->txdata (:policyDocument %)
                                   (:policyName %) "inline")
              (:rolePolicyList cfg))

        attached-txs
        (mapv (fn [{:keys [policyArn policyName]}]
                {:policy/arn  policyArn
                 :policy/name policyName
                 :policy/type "managed"})
              (:attachedManagedPolicies cfg))

        attached-refs
        (mapv #(vector :policy/arn (:policy/arn %)) attached-txs)

        role-tx
        (cond-> {:ci/arn             (:arn ci)
                 :ci/account-id      (:accountId ci)
                 :ci/region          (:awsRegion ci)
                 :ci/resource-type   (:resourceType ci)
                 :ci/resource-id     (:resourceId ci)
                 :ci/resource-name   (:resourceName ci)
                 :ci/status          (:configurationItemStatus ci)
                 :ci/capture-time    (str (:configurationItemCaptureTime ci))
                 :ci/config-state-id (str (:configurationStateId ci))
                 :role/id            (:roleId cfg)
                 :role/path          (:path cfg)
                 :role/create-date   (str (:createDate cfg))
                 :role/trust-policy  trust-ref
                 :role/inline        (mapv first inline-pairs)
                 :role/attached      attached-refs}
          (seq trusted-refs)
          (assoc :role/trusted-by trusted-refs)
          (:maxSessionDuration cfg)
          (assoc :role/max-session (:maxSessionDuration cfg))
          (get-in cfg [:roleLastUsed :lastUsedDate])
          (assoc :role/last-used-date   (str (get-in cfg [:roleLastUsed :lastUsedDate]))
                 :role/last-used-region (get-in cfg [:roleLastUsed :region]))
          (get-in cfg [:permissionsBoundary :permissionsBoundaryArn])
          (assoc :role/boundary-arn
                 (get-in cfg [:permissionsBoundary :permissionsBoundaryArn])))]

    (into trust-txs
          (concat (mapcat second inline-pairs)
                  attached-txs
                  [role-tx]))))

(defn- managed-policy-txdata [ci cfg]
  (let [latest-doc     (or (some-> (filter :isDefaultVersion (:policyVersionList cfg))
                                   first :document)
                           (:policyVersionList cfg))
        [_ stmts-tx]   (policy-doc->txdata latest-doc (:policyName cfg) "managed" (:arn cfg))
        ;; Find the policy entity in stmts-tx and merge CI envelope fields
        policy-tx      (-> (last stmts-tx)
                           (assoc :ci/arn          (:arn ci)
                                  :ci/account-id   (:accountId ci)
                                  :ci/region       (:awsRegion ci)
                                  :ci/resource-type (:resourceType ci)
                                  :ci/resource-id  (:resourceId ci)
                                  :ci/resource-name (:resourceName ci)
                                  :ci/status       (:configurationItemStatus ci)
                                  :ci/capture-time (str (:configurationItemCaptureTime ci))
                                  :ci/config-state-id (str (:configurationStateId ci))))]
    (conj (vec (butlast stmts-tx)) policy-tx)))

(defn ci->txdata
  "Convert an AWS Config CI map into Datalevin tx-data.
   :configuration may be a parsed map or a JSON string."
  [ci]
  (let [cfg (parse-json-field (:configuration ci))]
    (case (:resourceType ci)
      "AWS::IAM::Role"   (role-txdata ci cfg)
      "AWS::IAM::Policy" (managed-policy-txdata ci cfg)
      (throw (ex-info "Unsupported resourceType"
                      {:resourceType (:resourceType ci)})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write API
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ingest!
  "Upsert a single AWS Config CI. Idempotent — re-ingesting the same ARN updates in place."
  [ci]
  (d/transact! conn (ci->txdata ci)))

(defn ingest-many!
  "Upsert a collection of CIs. Trust relationships (:role/trusted-by) are
   applied in a second pass to handle forward references between roles."
  [cis]
  (let [all-tx   (vec (mapcat ci->txdata cis))
        base-tx  (mapv #(if (map? %) (dissoc % :role/trusted-by) %) all-tx)
        trust-tx (->> all-tx
                      (filter #(and (map? %) (contains? % :role/trusted-by)))
                      (mapv #(select-keys % [:ci/arn :role/trusted-by])))]
    (d/transact! conn base-tx)
    (when (seq trust-tx)
      (d/transact! conn trust-tx))))

(defn delete-by-arn!
  "Retract a CI entity by ARN."
  [arn]
  (when-let [eid (d/q '[:find ?e . :in $ ?arn :where [?e :ci/arn ?arn]]
                      (d/db conn) arn)]
    (d/transact! conn [[:db/retractEntity eid]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read API
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-by-arn
  "Pull full entity by ARN. Returns nil if not found."
  [arn]
  (when-let [eid (d/q '[:find ?e . :in $ ?arn :where [?e :ci/arn ?arn]]
                      (d/db conn) arn)]
    (d/pull (d/db conn) '[*] eid)))

(defn list-by-account
  "All CIs for a given account-id."
  [account-id]
  (d/q '[:find ?arn ?name ?type
         :keys arn name resource-type
         :in $ ?account
         :where
         [?e :ci/account-id ?account]
         [?e :ci/arn ?arn]
         [?e :ci/resource-name ?name]
         [?e :ci/resource-type ?type]]
       (d/db conn) account-id))

(defn list-roles
  "All IAM Role CIs, optionally filtered by account."
  ([]
   (d/q '[:find ?arn ?name ?acct
          :keys arn name account-id
          :where
          [?e :ci/resource-type "AWS::IAM::Role"]
          [?e :ci/arn ?arn]
          [?e :ci/resource-name ?name]
          [?e :ci/account-id ?acct]]
        (d/db conn)))
  ([account]
   (d/q '[:find ?arn ?name ?acct
          :keys arn name account-id
          :in $ ?acct-filter
          :where
          [?e :ci/resource-type "AWS::IAM::Role"]
          [?e :ci/arn ?arn]
          [?e :ci/resource-name ?name]
          [?e :ci/account-id ?acct]
          [(= ?acct ?acct-filter)]]
        (d/db conn) account)))

(defn roles-assuming-service
  "Roles whose trust policy allows a given service to assume them.
   Example: (roles-assuming-service \"lambda.amazonaws.com\")"
  [service]
  (d/q '[:find ?name ?arn
         :keys name arn
         :in $ ?svc
         :where
         [?r :ci/resource-type "AWS::IAM::Role"]
         [?r :ci/resource-name ?name]
         [?r :ci/arn ?arn]
         [?r :role/trust-policy ?tp]
         [?tp :policy/statements ?s]
         [?s :stmt/principals ?p]
         [(clojure.string/includes? ?p ?svc)]]
       (d/db conn) service))

(defn roles-with-attached-policy
  "Roles that have a specific managed policy ARN attached."
  [policy-arn]
  (d/q '[:find ?name ?arn
         :keys name arn
         :in $ ?parn
         :where
         [?r :role/attached ?p]
         [?p :policy/arn ?parn]
         [?r :ci/resource-name ?name]
         [?r :ci/arn ?arn]]
       (d/db conn) policy-arn))

(defn roles-trusting
  "Roles that trust the given role (i.e., the given role can assume them).
   Traverses :role/trusted-by in reverse."
  [assumer-arn]
  (d/q '[:find ?name ?arn
         :keys name arn
         :in $ ?assumer
         :where
         [?a :ci/arn ?assumer]
         [?r :role/trusted-by ?a]
         [?r :ci/resource-name ?name]
         [?r :ci/arn ?arn]]
       (d/db conn) assumer-arn))

(defn roles-trusted-by
  "Roles trusted by the given role (i.e., roles that can assume it).
   Traverses :role/trusted-by forward."
  [target-arn]
  (d/q '[:find ?name ?arn
         :keys name arn
         :in $ ?target
         :where
         [?t :ci/arn ?target]
         [?t :role/trusted-by ?a]
         [?a :ci/resource-name ?name]
         [?a :ci/arn ?arn]]
       (d/db conn) target-arn))

(defn role-allowed-actions
  "All Allow actions granted to a role via inline and attached policies."
  [role-arn]
  (->> (d/q '[:find ?action
              :in $ ?arn
              :where
              [?r :ci/arn ?arn]
              (or [?r :role/inline ?p]
                  [?r :role/attached ?p])
              [?p :policy/statements ?s]
              [?s :stmt/effect "Allow"]
              [?s :stmt/actions ?action]]
            (d/db conn) role-arn)
       (map first)
       sort vec))

(def ^:private policy-rules
  '[[(role-policy ?role ?policy)
     [?role :role/attached ?policy]]
    [(role-policy ?role ?policy)
     [?role :role/inline ?policy]]
    [(role-policy ?role ?policy)
     [?role :role/trust-policy ?policy]]])

(defn role-all-policies
  "All policies reachable from a role (trust + inline + attached)."
  [role-arn]
  (d/q '[:find ?name ?type
         :keys name type
         :in $ % ?arn
         :where
         [?r :ci/arn ?arn]
         (role-policy ?r ?p)
         [?p :policy/name ?name]
         [?p :policy/type ?type]]
       (d/db conn) policy-rules role-arn))

(defn trust-graph
  "All role-to-role trust edges.
   Returns #{:assumer :assumer-arn :target :target-arn} where assumer can assume target."
  []
  (d/q '[:find ?aname ?aarn ?tname ?tarn
         :keys assumer assumer-arn target target-arn
         :where
         [?t :role/trusted-by ?a]
         [?t :ci/resource-name ?tname]
         [?t :ci/arn ?tarn]
         [?a :ci/resource-name ?aname]
         [?a :ci/arn ?aarn]]
       (d/db conn)))

(defn trust-chain
  "Transitive trust: all roles reachable by assuming from the given role ARN.
   Single recursive pull via :role/_trusted-by (reverse ref).
   Returns #{:name :arn :depth}."
  [assumer-arn]
  (let [tree (d/pull (d/db conn)
                     '[:ci/arn :ci/resource-name {:role/_trusted-by ...}]
                     [:ci/arn assumer-arn])
        walk (fn walk [children depth]
               (mapcat (fn [node]
                         (when-let [arn (:ci/arn node)]
                           (cons {:arn  arn
                                  :name (:ci/resource-name node)
                                  :depth depth}
                                 (walk (:role/_trusted-by node) (inc depth)))))
                       children))]
    (set (walk (:role/_trusted-by tree) 1))))

(defn policy-attachments
  "All managed-policy-to-role attachment pairs.
   Returns #{:policy-arn :policy-name :role-arn :role-name}."
  []
  (d/q '[:find ?parn ?pname ?rarn ?rname
         :keys policy-arn policy-name role-arn role-name
         :where
         [?r :role/attached ?p]
         [?p :policy/arn ?parn]
         [?p :policy/name ?pname]
         [?r :ci/arn ?rarn]
         [?r :ci/resource-name ?rname]]
       (d/db conn)))

(defn policies-by-action
  "All managed policies that Allow a given action prefix (e.g. \"s3:\")."
  [action-prefix]
  (d/q '[:find ?name ?arn
         :keys name arn
         :in $ ?prefix
         :where
         [?p :policy/type "managed"]
         [?p :policy/name ?name]
         [?p :policy/arn ?arn]
         [?p :policy/statements ?s]
         [?s :stmt/effect "Allow"]
         [?s :stmt/actions ?action]
         [(clojure.string/starts-with? ?action ?prefix)]]
       (d/db conn) action-prefix))
