(ns iam-oracle
  (:require
   [clojure.string :as str]
   [cheshire.core :as json]
   #?(:bb  [pod.huahaiy.datalevin :as d]
      :clj [datalevin.core :as d])))

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
   :stmt/condition      {:db/valueType :db.type/string}   ; serialized JSON

   ;; ── IAM Action (from servicespec) ────────────────────────────────────
   :action/id            {:db/unique    :db.unique/identity
                          :db/valueType :db.type/string}
   :action/service       {:db/valueType :db.type/string}
   :action/name          {:db/valueType :db.type/string}
   :action/verb          {:db/valueType :db.type/string}
   ;; "Read"|"List"|"Write"|"PermissionManagement"|"Tagging"
   ;; Single string avoids the pod boolean-index bug.
   :action/access-level  {:db/valueType :db.type/string}
   :action/resource-types {:db/valueType   :db.type/ref
                           :db/cardinality :db.cardinality/many}

   ;; ── Resource Type (from servicespec) ─────────────────────────────────
   :resource-type/id              {:db/unique    :db.unique/identity
                                   :db/valueType :db.type/string}
   :resource-type/service         {:db/valueType :db.type/string}
   :resource-type/name            {:db/valueType :db.type/string}
   :resource-type/arn-format      {:db/valueType :db.type/string}
   :resource-type/resource-prefix {:db/valueType :db.type/string}})

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def db-path (or (System/getenv "IAM_ORACLE_DB") "/tmp/iam-oracle.dtlv"))

(def data-dir (or (System/getenv "IAM_ORACLE_DATA") "../aws/iam/"))

(defonce conn (d/get-conn db-path schema))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-json-field [v]
  (cond (map? v) v (string? v) (json/parse-string v true) :else nil))

(defn- ensure-vec [x]
  (cond (nil? x) [] (sequential? x) (vec x) :else [x]))

(defn- uuid-str [] (str (random-uuid)))

(defn- deterministic-id
  "SHA-256 based deterministic ID from seed strings."
  [& parts]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")
        bytes  (.digest digest (.getBytes (str/join "|" parts) "UTF-8"))]
    (str/join (map #(format "%02x" (bit-and % 0xff)) (take 16 bytes)))))

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
   Returns [lookup-refs tx-maps] where lookup-refs are [:stmt/id id] vectors.
   When id-seed is provided, statement IDs are deterministic (seed + index)."
  ([statements] (statements->txdata statements nil))
  ([statements id-seed]
   (let [[refs txs _] (reduce
                       (fn [[refs txs idx] stmt]
                         (let [sid         (if id-seed (deterministic-id id-seed "stmt" (str idx)) (uuid-str))
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
                           [(conj refs [:stmt/id sid]) (conj txs tx) (inc idx)]))
                       [[] [] 0]
                       statements)]
     [refs txs])))

(defn- policy-doc->txdata
  "Build tx-data for a policy document.
   Returns [lookup-ref all-tx-maps] where lookup-ref is [:policy/id id] or [:policy/arn arn].
   When id-seed is provided, policy and statement IDs are deterministic."
  [policy-doc policy-name policy-type & [managed-arn id-seed]]
  (let [doc            (parse-json-field policy-doc)
        stmts          (ensure-vec (:Statement doc))
        policy-seed    (or id-seed (when managed-arn managed-arn))
        [srefs stmts-tx] (statements->txdata stmts policy-seed)
        pid            (if policy-seed (deterministic-id policy-seed) (uuid-str))
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
  (let [role-arn (:arn ci)

        [trust-ref trust-txs]
        (policy-doc->txdata (:assumeRolePolicyDocument cfg) "trust" "trust"
                            nil (str role-arn "|trust"))

        trusted-role-arns
        (extract-trusted-role-arns (:assumeRolePolicyDocument cfg))

        trusted-refs
        (mapv #(vector :ci/arn %) trusted-role-arns)

        inline-pairs
        (mapv #(policy-doc->txdata (:policyDocument %)
                                   (:policyName %) "inline"
                                   nil (str role-arn "|inline|" (:policyName %)))
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
  "Upsert a single AWS Config CI. Idempotent and commutative — ingests the
   entity first (without trust edges), then:
   1. Forward: links this role's :role/trusted-by to existing ARNs
   2. Reverse: finds existing roles whose trust policy principals include
      this ARN and adds the missing trust edges to them
   Order of ingestion doesn't matter."
  [ci]
  (let [all-tx   (ci->txdata ci)
        base-tx  (mapv #(if (map? %) (dissoc % :role/trusted-by) %) all-tx)
        trust-tx (->> all-tx
                      (filter #(and (map? %) (contains? % :role/trusted-by)))
                      (mapv #(select-keys % [:ci/arn :role/trusted-by])))]
    ;; 1. Base entity
    (d/transact! conn base-tx)
    ;; 2. Forward: link this role's trusted-by to known ARNs
    (when (seq trust-tx)
      (let [known-arns (into #{}
                             (d/q '[:find [?arn ...] :where [?e :ci/arn ?arn]]
                                  (d/db conn)))
            filtered   (->> trust-tx
                            (mapv (fn [tx]
                                    (update tx :role/trusted-by
                                            (fn [refs]
                                              (filterv #(contains? known-arns (second %)) refs)))))
                            (filterv #(seq (:role/trusted-by %))))]
        (when (seq filtered)
          (d/transact! conn filtered))))
    ;; 3. Reverse: existing roles whose trust policy lists this ARN as principal
    (when (role-arn? (:arn ci))
      (let [new-arn   (:arn ci)
            referrers (d/q '[:find [?rarn ...]
                             :in $ ?new-arn
                             :where
                             [?r :role/trust-policy ?tp]
                             [?tp :policy/statements ?s]
                             [?s :stmt/principals ?new-arn]
                             [?r :ci/arn ?rarn]]
                           (d/db conn) new-arn)
            reverse-tx (mapv #(hash-map :ci/arn %
                                        :role/trusted-by [[:ci/arn new-arn]])
                             referrers)]
        (when (seq reverse-tx)
          (d/transact! conn reverse-tx))))))

(defn ingest-many!
  "Upsert a collection of CIs. Calls ingest! for each, so trust edges
   accumulate as entities become available."
  [cis]
  (run! ingest! cis))

(defn delete-by-arn!
  "Retract a CI entity by ARN."
  [arn]
  (when-let [eid (d/q '[:find ?e . :in $ ?arn :where [?e :ci/arn ?arn]]
                      (d/db conn) arn)]
    (d/transact! conn [[:db/retractEntity eid]])))

(defn gc!
  "Garbage-collect orphaned policy and statement entities not referenced by any role.
   Returns {:policies n :statements m} counts of retracted entities."
  []
  (let [db (d/db conn)
        ;; All policy eids referenced by some role
        referenced-policies
        (into #{}
              (d/q '[:find [?p ...]
                     :where
                     (or [_ :role/attached ?p]
                         [_ :role/inline ?p]
                         [_ :role/trust-policy ?p])]
                   db))
        ;; All policy eids in DB
        all-policies
        (into #{}
              (d/q '[:find [?p ...]
                     :where [?p :policy/id _]]
                   db))
        orphan-policies (remove referenced-policies all-policies)
        ;; All statement eids referenced by some policy
        referenced-stmts
        (into #{}
              (d/q '[:find [?s ...]
                     :where [_ :policy/statements ?s]]
                   db))
        all-stmts
        (into #{}
              (d/q '[:find [?s ...]
                     :where [?s :stmt/id _]]
                   db))
        orphan-stmts (remove referenced-stmts all-stmts)
        retractions  (into (mapv #(vector :db/retractEntity %) orphan-policies)
                           (mapv #(vector :db/retractEntity %) orphan-stmts))]
    (when (seq retractions)
      (d/transact! conn retractions))
    {:policies (count orphan-policies) :statements (count orphan-stmts)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Servicespec ingest
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- derive-access-level
  [{:keys [IsList IsWrite IsPermissionManagement IsTaggingOnly]}]
  (cond
    IsPermissionManagement "PermissionManagement"
    IsTaggingOnly          "Tagging"
    IsWrite                "Write"
    IsList                 "List"
    :else                  "Read"))

(defn- extract-verb [action-name]
  (or (second (re-find #"^([A-Z][a-z]+)" action-name))
      action-name))

(defn- extract-resource-prefix
  "Literal prefix of the resource segment before the first ${...} variable.
   e.g. 'role/' from 'arn:...:role/${RoleName}',
        'function:' from 'arn:...:function:${FunctionName}',
        '' from 'arn:...:${BucketName}'."
  [arn-format]
  (let [parts        (str/split arn-format #":" 6)
        resource-seg (nth parts 5 "")]
    (or (second (re-find #"^([^$]*)" resource-seg)) "")))

(defn load-servicespec!
  "Load a servicespec JSON file into the database. Idempotent — upserts via
   :action/id and :resource-type/id identity attributes.
   Returns the count of actions loaded."
  [path service-name]
  (let [spec (json/parse-string (slurp path) true)
        known-rts (into #{} (map #(str service-name ":" (:Name %))) (:Resources spec))
        rt-txs (mapv (fn [r]
                       (let [arn-fmt (first (:ARNFormats r))]
                         {:resource-type/id              (str service-name ":" (:Name r))
                          :resource-type/service         service-name
                          :resource-type/name            (:Name r)
                          :resource-type/arn-format      arn-fmt
                          :resource-type/resource-prefix (extract-resource-prefix arn-fmt)}))
                     (:Resources spec))
        action-txs (mapv (fn [a]
                           (let [action-name (:Name a)
                                 props       (get-in a [:Annotations :Properties])
                                 res-refs    (when-let [rs (seq (:Resources a))]
                                               (->> rs
                                                    (map #(str service-name ":" (:Name %)))
                                                    (filter known-rts)
                                                    (mapv #(vector :resource-type/id %))))]
                             (cond-> {:action/id           (str service-name ":" action-name)
                                      :action/service      service-name
                                      :action/name         action-name
                                      :action/verb         (extract-verb action-name)
                                      :action/access-level (derive-access-level props)}
                               (seq res-refs) (assoc :action/resource-types res-refs))))
                         (:Actions spec))]
    (d/transact! conn (into rt-txs action-txs))
    (count action-txs)))

(defn load-all-servicespecs!
  "Load all servicespec JSON files from specs-dir.
   Derives service name from filename (e.g. 's3.json' → 's3')."
  [specs-dir]
  (let [files (->> (file-seq (clojure.java.io/file specs-dir))
                   (filter #(str/ends-with? (.getName %) ".json"))
                   (sort-by #(.getName %)))]
    (doseq [f files]
      (let [svc (str/replace (.getName f) #"\.json$" "")
            n   (load-servicespec! (str f) svc)]
        (println (str "  " svc ": " n " actions"))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Servicespec rules
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private action-rules
  '[;; readonly = Read or List (two heads = logical OR)
    [(readonly-action ?e)
     [?e :action/access-level "Read"]]
    [(readonly-action ?e)
     [?e :action/access-level "List"]]
    ;; actions by service + verb
    [(verb-actions ?svc ?verb ?e)
     [?e :action/service ?svc]
     [?e :action/verb ?verb]]])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Servicespec queries
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-action?
  "Check if an action string exists in the servicespec (or is a wildcard).
   Returns truthy if valid."
  [action-str]
  (or (= action-str "*")
      (and (string? action-str)
           (re-matches #"[a-z0-9-]+:\*" action-str))
      (some? (d/q '[:find ?e . :in $ ?id :where [?e :action/id ?id]]
                   (d/db conn) action-str))))

(defn invalid-actions
  "Return actions from the collection that don't exist in the servicespec."
  [actions]
  (vec (remove valid-action? actions)))

(defn expand-action-pattern
  "Expand a wildcard action pattern to matching action IDs.
   Supports: \"*\", \"s3:*\", \"s3:Get*\", \"s3:GetObj*\", \"s3:GetObject\"."
  [pattern]
  (let [db (d/db conn)]
    (cond
      (= pattern "*")
      (d/q '[:find [?id ...] :where [_ :action/id ?id]] db)

      ;; "s3:*" — all actions for a service
      (re-matches #"[a-z0-9-]+:\*" pattern)
      (let [svc (first (str/split pattern #":"))]
        (d/q '[:find [?id ...] :in $ ?svc
               :where [?e :action/service ?svc] [?e :action/id ?id]]
             db svc))

      ;; "s3:Get*" — verb-level wildcard (indexed via :action/verb)
      (re-matches #"[a-z0-9-]+:[A-Z][a-z]+\*" pattern)
      (let [[svc action-pat] (str/split pattern #":" 2)
            verb (str/replace action-pat "*" "")]
        (d/q '[:find [?id ...] :in $ % ?svc ?verb
               :where (verb-actions ?svc ?verb ?e) [?e :action/id ?id]]
             db action-rules svc verb))

      ;; "s3:GetObj*" — sub-verb prefix (predicate, full ns required for pod)
      (str/ends-with? pattern "*")
      (let [[svc action-pat] (str/split pattern #":" 2)
            prefix (str/replace action-pat #"\*$" "")]
        (d/q '[:find [?id ...] :in $ ?svc ?prefix
               :where [?e :action/service ?svc]
                      [?e :action/name ?name]
                      [(clojure.string/starts-with? ?name ?prefix)]
                      [?e :action/id ?id]]
             db svc prefix))

      ;; literal action name
      :else
      (if (d/q '[:find ?e . :in $ ?id :where [?e :action/id ?id]] db pattern)
        [pattern]
        []))))

(defn action-pattern-readonly?
  "Check if ALL actions matching a wildcard pattern are readonly (Read or List).
   Returns true/false, or nil if no actions match."
  [pattern]
  (let [ids (expand-action-pattern pattern)]
    (when (seq ids)
      (let [levels (d/q '[:find [?al ...]
                          :in $ [?id ...]
                          :where [?e :action/id ?id]
                                 [?e :action/access-level ?al]]
                        (d/db conn) ids)]
        (every? #{"Read" "List"} levels)))))

(defn- normalize-level
  "Merge Tagging into Write for output grouping."
  [al]
  (if (= al "Tagging") "Write" al))

(defn compress-actions
  "Compress a list of action IDs into a map of access-level to action list,
   collapsing actions that share service + verb + access-level into Verb* wildcards.
   Tagging is merged into Write. Only compresses when ALL input actions for that
   service+verb have the same normalized access level."
  [action-ids]
  (let [rows (->> (d/q '[:find ?svc ?verb ?al ?id
                         :in $ [?id ...]
                         :where
                         [?e :action/id ?id]
                         [?e :action/service ?svc]
                         [?e :action/verb ?verb]
                         [?e :action/access-level ?al]]
                       (d/db conn) action-ids)
                  (mapv (fn [[svc verb al id]]
                          [svc verb (normalize-level al) id])))
        ;; Which [svc verb] pairs have a single access level across all input actions?
        safe-to-compress
        (->> rows
             (group-by (fn [[svc verb _ _]] [svc verb]))
             (into #{}
                   (comp (filter (fn [[_ grp]]
                                   (= 1 (count (into #{} (map #(nth % 2)) grp)))))
                         (map key))))]
    (->> rows
         (group-by (fn [[_ _ al _]] al))
         (into {}
               (map (fn [[al members]]
                      [al (->> members
                                (group-by (fn [[svc verb _ _]] [svc verb]))
                                (mapcat (fn [[[svc verb :as k] grp]]
                                          (if (and (> (count grp) 1)
                                                   (contains? safe-to-compress k))
                                            [(str svc ":" verb "*")]
                                            (mapv #(nth % 3) grp))))
                                vec)]))))))

(defn- title-case
  "\"secretsmanager\" → \"Secretsmanager\"."
  [s]
  (str (str/upper-case (subs s 0 1)) (subs s 1)))

(defn- svc-of
  "\"s3:GetObject\" → \"s3\", \"arn:aws:s3:::bucket\" → \"s3\", \"*\" → nil."
  [s]
  (cond
    (str/starts-with? s "arn:") (nth (str/split s #":" 4) 2 nil)
    (str/includes? s ":")       (subs s 0 (str/index-of s ":"))
    :else                       nil))

(defn- resolve-statement
  "Resolve input to a statement map with string keys.
   Accepts JSON string or :stmt/id string for Datalevin lookup."
  [stmt-or-json]
  (if (and (string? stmt-or-json)
           (not (str/starts-with? (str/trim stmt-or-json) "{")))
    (let [ent (d/pull (d/db conn) '[*] [:stmt/id stmt-or-json])]
      (when ent
        (cond-> {"Effect"   (:stmt/effect ent)
                 "Action"   (vec (:stmt/actions ent))
                 "Resource" (vec (:stmt/resources ent))}
          (:stmt/sid ent)        (assoc "Sid" (:stmt/sid ent))
          (:stmt/condition ent)  (assoc "Condition"
                                        (json/parse-string (:stmt/condition ent)))
          (:stmt/principals ent) (assoc "Principal"
                                        {(:stmt/principal-type ent)
                                         (vec (:stmt/principals ent))}))))
    (json/parse-string stmt-or-json)))

(defn- classify-wildcard
  "Return the access level for a wildcard action pattern, or \"Mixed\"/\"Unknown\"."
  [wc]
  (if (= wc "*")
    "Mixed"
    (let [expanded (expand-action-pattern wc)]
      (if (empty? expanded)
        "Unknown"
        (let [levels (into #{} (map normalize-level)
                           (d/q '[:find [?al ...]
                                  :in $ [?id ...]
                                  :where [?e :action/id ?id]
                                         [?e :action/access-level ?al]]
                                (d/db conn) expanded))]
          (if (= 1 (count levels)) (first levels) "Mixed"))))))

(defn split-statement-by-access-level
  "Split a single-service policy statement into statements grouped by access level.
   Input: JSON string or :stmt/id for Datalevin lookup.
   Returns input as singleton list when:
   - Actions+resources span multiple services
   - Statement uses NotAction or NotResource (ambiguous semantics)
   Otherwise compresses actions within each level and generates
   Sid as {OriginalSid}{Service}{AccessLevel}."
  [stmt-or-json]
  (let [stmt       (resolve-statement stmt-or-json)
        _          (assert stmt "Could not resolve statement")
        ;; Bail on NotAction / NotResource — semantics are inverted
        not-action?   (contains? stmt "NotAction")
        not-resource? (contains? stmt "NotResource")
        actions    (ensure-vec (or (get stmt "Action") (get stmt "NotAction")))
        resources  (ensure-vec (or (get stmt "Resource") (get stmt "NotResource")))
        svcs       (->> (concat actions resources) (keep svc-of) (into #{}))]
    (if (or not-action? not-resource? (> (count svcs) 1))
      ;; Unsplittable → return as-is
      [stmt]
      ;; Single-service → split by access level
      (let [orig-sid    (get stmt "Sid" "")
            base-fields (dissoc stmt "Action" "Sid")

            {wildcards true concretes false} (group-by #(str/includes? % "*") actions)
            known-set   (when (seq concretes)
                          (into #{} (d/q '[:find [?id ...]
                                           :in $ [?id ...]
                                           :where [?e :action/id ?id]]
                                         (d/db conn) concretes)))
            {known true unknown false} (group-by #(contains? (or known-set #{}) %) concretes)

            wc-levels (reduce (fn [m wc]
                                (update m (classify-wildcard wc) (fnil conj []) wc))
                              {} wildcards)
            level-map (merge-with into
                                  (if (seq known) (compress-actions known) {})
                                  wc-levels
                                  (when (seq unknown) {"Unknown" unknown}))]
        (->> level-map
             (sort-by key)
             (mapv (fn [[level acts]]
                     (let [svc (->> acts (keep svc-of) first)]
                       (assoc base-fields
                              "Sid"    (str orig-sid (when svc (title-case svc)) level)
                              "Action" (vec (sort acts)))))))))))

(defn- arn-has-region?
  "Derive from the ARN format template whether the region segment is non-empty."
  [arn-format]
  (not (str/blank? (nth (str/split arn-format #":" 6) 3 ""))))

(defn- arn-has-account?
  "Derive from the ARN format template whether the account segment is non-empty."
  [arn-format]
  (not (str/blank? (nth (str/split arn-format #":" 6) 4 ""))))

(defn valid-resource-for-action?
  "Check if a resource ARN is structurally valid for a given action.
   Validates service match, region/account emptiness, and resource prefix."
  [action-str resource-arn]
  (or (= resource-arn "*")
      (let [db      (d/db conn)
            rt-data (d/q '[:find ?rt-svc ?rt-prefix ?rt-arn-fmt
                           :in $ ?action-id
                           :where
                           [?a :action/id ?action-id]
                           [?a :action/resource-types ?rt]
                           [?rt :resource-type/service ?rt-svc]
                           [?rt :resource-type/resource-prefix ?rt-prefix]
                           [?rt :resource-type/arn-format ?rt-arn-fmt]]
                         db action-str)]
        (if (empty? rt-data)
          ;; action has no resource type constraint — only * is valid
          (= resource-arn "*")
          ;; check if ARN structurally matches any resource type
          (let [arn-parts    (str/split resource-arn #":" 6)
                arn-svc      (nth arn-parts 2 "")
                arn-region   (nth arn-parts 3 "")
                arn-account  (nth arn-parts 4 "")
                arn-resource (nth arn-parts 5 "")]
            (boolean
             (some (fn [[rt-svc rt-prefix rt-arn-fmt]]
                     (and
                      (= arn-svc rt-svc)
                      (if (arn-has-region? rt-arn-fmt)
                        (not (str/blank? arn-region))
                        (str/blank? arn-region))
                      (if (arn-has-account? rt-arn-fmt)
                        (not (str/blank? arn-account))
                        (str/blank? arn-account))
                      (or (= arn-resource "*")
                          (str/blank? rt-prefix)
                          (str/starts-with? arn-resource rt-prefix))))
                   rt-data)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DB initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- db-populated?
  "Check if the database already has action data loaded."
  []
  (some? (d/q '[:find ?e . :where [?e :action/id _]] (d/db conn))))

(defn init-db!
  "Populate the database with servicespecs and sample CIs if empty.
   Idempotent — skips loading when data already exists.
   specs-dir: path to servicespec JSON files (default: ../aws/iam/servicespec/)
   samples-dir: path to sample CI JSON files (default: ../aws/iam/samples/)"
  ([] (init-db! (str data-dir "servicespec/") (str data-dir "samples/")))
  ([specs-dir samples-dir]
   (if (db-populated?)
     (println "DB already populated, skipping init.")
     (do
       (println "Loading servicespecs...")
       (load-all-servicespecs! specs-dir)
       (println "Loading sample CIs...")
       (let [samples (->> (file-seq (clojure.java.io/file samples-dir))
                          (filter #(str/ends-with? (.getName %) ".json"))
                          (sort-by #(.getName %)))]
         (ingest-many!
           (mapv (fn [f]
                   (let [ci (json/parse-string (slurp f) true)]
                     (println (str "  " (.getName f) " → " (:resourceType ci)))
                     ci))
                 samples)))
       (println "Done. Loaded from" specs-dir "and" samples-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI wrappers (bb -x)
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ->json [x]
  (println (json/generate-string x {:pretty true})))

(defn- read-stdin-jsonl
  "Read stdin as JSONL (one compact JSON object per line).
   Use with: jq -c . file.json | bb -x iam-oracle/load-fact"
  []
  (->> (line-seq (clojure.java.io/reader *in*))
       (remove str/blank?)
       (mapv #(json/parse-string % true))))

(defn load-fact
  "Ingest AWS Config CI JSON from stdin.
   cat ci.json | bb -x iam-oracle/load-fact       # single CI
   cat *.json  | bb -x iam-oracle/load-fact        # JSONL"
  [_opts]
  (let [cis (read-stdin-jsonl)]
    (if (= 1 (count cis))
      (do (ingest! (first cis))
          (println "Ingested" (:arn (first cis))))
      (do (ingest-many! cis)
          (println "Ingested" (count cis) "CIs")))))

(defn tidy-policy
  "Split an IAM policy statement JSON by access level.
   echo '{\"Effect\":\"Allow\",...}' | bb -x iam-oracle/tidy-policy"
  [_opts]
  (->json (split-statement-by-access-level (slurp *in*))))

;; ── Trust graph ─────────────────────────────────────────────────────────────

(defn who-trusts
  "Roles that trust this role (it can assume them).
   bb -x iam-oracle/who-trusts --arn ARN"
  [{:keys [arn]}]
  (->json (roles-trusting arn)))

(defn trusted-by
  "Roles trusted by this role (they can assume it).
   bb -x iam-oracle/trusted-by --arn ARN"
  [{:keys [arn]}]
  (->json (roles-trusted-by arn)))

(defn chain
  "Transitive trust: all roles reachable from a role.
   bb -x iam-oracle/chain --arn ARN"
  [{:keys [arn]}]
  (->json (trust-chain arn)))

(defn graph
  "Full role→role trust graph.
   bb -x iam-oracle/graph"
  [_opts]
  (->json (trust-graph)))

(defn by-service
  "Roles whose trust policy allows a service to assume them.
   bb -x iam-oracle/by-service --service lambda.amazonaws.com"
  [{:keys [service]}]
  (->json (roles-assuming-service service)))

;; ── Policies & actions ──────────────────────────────────────────────────────

(defn policies
  "All policies on a role (inline + attached + trust).
   bb -x iam-oracle/policies --arn ARN"
  [{:keys [arn]}]
  (->json (role-all-policies arn)))

(defn actions
  "All Allow actions for a role.
   bb -x iam-oracle/actions --arn ARN"
  [{:keys [arn]}]
  (->json (role-allowed-actions arn)))

(defn with-policy
  "Roles that have a managed policy attached.
   bb -x iam-oracle/with-policy --arn POLICY_ARN"
  [{:keys [arn]}]
  (->json (roles-with-attached-policy arn)))

(defn attachments
  "All managed-policy↔role attachment pairs.
   bb -x iam-oracle/attachments"
  [_opts]
  (->json (policy-attachments)))

(defn find-policies
  "Managed policies that allow actions matching a prefix.
   bb -x iam-oracle/find-policies --prefix s3:"
  [{:keys [prefix]}]
  (->json (policies-by-action prefix)))

;; ── Action validation ───────────────────────────────────────────────────────

(defn check-actions
  "Print invalid actions from a JSON array on stdin.
   echo '[\"s3:GetObject\",\"s3:GettObject\"]' | bb -x iam-oracle/check-actions"
  [_opts]
  (->json (invalid-actions (json/parse-string (slurp *in*)))))

(defn expand
  "Expand a wildcard action pattern to matching action IDs.
   bb -x iam-oracle/expand --pattern 's3:Get*'"
  [{:keys [pattern]}]
  (->json (expand-action-pattern pattern)))

(defn is-readonly
  "Check if all actions matching a pattern are readonly.
   bb -x iam-oracle/is-readonly --pattern 's3:Get*'"
  [{:keys [pattern]}]
  (->json (action-pattern-readonly? pattern)))

(defn check-resource
  "Check if a resource ARN is valid for an action.
   bb -x iam-oracle/check-resource --action s3:GetObject --resource 'arn:aws:s3:::bucket/*'"
  [{:keys [action resource]}]
  (->json (valid-resource-for-action? action resource)))

(defn compress
  "Compress action IDs by access level. Reads JSON array from stdin.
   echo '[\"s3:GetObject\",\"s3:PutObject\"]' | bb -x iam-oracle/compress"
  [_opts]
  (->json (compress-actions (json/parse-string (slurp *in*)))))

;; ── Entity lookup ───────────────────────────────────────────────────────────

(defn lookup
  "Pull full entity by ARN.
   bb -x iam-oracle/lookup --arn ARN"
  [{:keys [arn]}]
  (->json (get-by-arn arn)))

(defn roles
  "List all IAM roles, optionally filtered by account.
   bb -x iam-oracle/roles
   bb -x iam-oracle/roles --account 123456789012"
  [{:keys [account]}]
  (->json (if account (list-roles (str account)) (list-roles))))

(defn by-account
  "List all CIs for an account.
   bb -x iam-oracle/by-account --account 123456789012"
  [{:keys [account]}]
  (->json (list-by-account (str account))))
