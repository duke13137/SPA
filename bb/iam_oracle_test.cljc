(ns iam-oracle-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [cheshire.core :as json]
            [iam-oracle :as sut]
            #?(:bb  [pod.huahaiy.datalevin :as d]
               :clj [datalevin.core :as d])))

;;; ─── DB isolation ────────────────────────────────────────────────────────────
;;; Each property trial gets a fresh, isolated Datalevin instance by temporarily
;;; rebinding sut/conn. The defonce in iam-oracle.cljc runs once on load (opening
;;; /tmp/bb-iam-server) but every test body sees a private temp connection.

(defmacro with-fresh-db [& body]
  `(let [path# (str "/tmp/bb-iam-test-" (random-uuid))
         c#    (d/get-conn path# sut/schema)]
     (try
       (with-redefs [sut/conn c#]
         ~@body)
       (finally
         (d/close c#)))))

;;; ─── Primitive generators ────────────────────────────────────────────────────

(def gen-account-id
  "12-digit numeric string, like a real AWS account ID."
  (gen/fmap #(format "%012d" (Math/abs (long %)))
            gen/large-integer))

(def gen-resource-name
  "Alphanumeric name, 3–20 chars."
  (gen/such-that #(>= (count %) 3)
                 (gen/resize 20 gen/string-alphanumeric)
                 100))

(def gen-service
  (gen/elements ["lambda.amazonaws.com"
                 "ec2.amazonaws.com"
                 "codebuild.amazonaws.com"
                 "ecs-tasks.amazonaws.com"]))

(def ^:private real-actions
  (let [specs-dir "/Users/duke/dev/SPA/aws/iam/servicespec/"
        services  ["s3" "ec2" "lambda" "iam" "logs" "sts" "dynamodb" "secretsmanager"]]
    (->> services
         (mapcat (fn [svc]
                   (let [f      (str specs-dir svc ".json")
                         parsed (json/parse-string (slurp f) true)]
                     (map #(str svc ":" (:Name %)) (:Actions parsed)))))
         vec)))

(def gen-action
  (gen/one-of [(gen/elements real-actions)
               (gen/return "*")]))

(def gen-role-arn
  (gen/let [acct gen-account-id
            name gen-resource-name]
    (str "arn:aws:iam::" acct ":role/" name)))

(def gen-policy-arn
  (gen/let [acct gen-account-id
            name gen-resource-name]
    (str "arn:aws:iam::" acct ":policy/" name)))

;;; Two distinct ARNs — used wherever a trust relationship requires two parties.
(def gen-two-role-arns
  (gen/such-that (fn [[a b]] (not= a b))
                 (gen/tuple gen-role-arn gen-role-arn)
                 100))

;;; Two distinct account IDs — used to test account isolation.
(def gen-two-account-ids
  (gen/such-that (fn [[a b]] (not= a b))
                 (gen/tuple gen-account-id gen-account-id)
                 100))

;;; ─── Document generators (JSON strings) ─────────────────────────────────────

(defn trust-doc-for-service [svc]
  (json/generate-string
   {"Version"   "2012-10-17"
    "Statement" [{"Effect"    "Allow"
                  "Principal" {"Service" svc}
                  "Action"    "sts:AssumeRole"}]}))

(defn trust-doc-for-role [role-arn]
  (json/generate-string
   {"Version"   "2012-10-17"
    "Statement" [{"Effect"    "Allow"
                  "Principal" {"AWS" role-arn}
                  "Action"    "sts:AssumeRole"}]}))

(def gen-trust-doc-service
  (gen/let [svc gen-service]
    (trust-doc-for-service svc)))

(def gen-allow-policy-doc
  (gen/let [actions (gen/vector gen-action 1 3)]
    (json/generate-string
     {"Version"   "2012-10-17"
      "Statement" [{"Effect"   "Allow"
                    "Action"   actions
                    "Resource" "*"}]})))

(def gen-deny-policy-doc
  "Policy document with only Deny statements — no Allow actions."
  (gen/let [actions (gen/vector gen-action 1 3)]
    (json/generate-string
     {"Version"   "2012-10-17"
      "Statement" [{"Effect"   "Deny"
                    "Action"   actions
                    "Resource" "*"}]})))

;;; ─── CI map builders ─────────────────────────────────────────────────────────

(defn make-role-ci
  "Minimal AWS Config CI map for AWS::IAM::Role."
  [arn account-id trust-doc]
  {:arn                          arn
   :accountId                    account-id
   :awsRegion                    "us-east-1"
   :resourceType                 "AWS::IAM::Role"
   :resourceId                   (str "AROA" (format "%08x" (Math/abs (long (hash arn)))))
   :resourceName                 (last (str/split arn #"/"))
   :configurationItemStatus      "OK"
   :configurationItemCaptureTime "2024-01-01T00:00:00Z"
   :configurationStateId         "1"
   :configuration                {:roleId                  "AROATEST123"
                                  :path                    "/"
                                  :createDate              "2024-01-01"
                                  :assumeRolePolicyDocument trust-doc
                                  :rolePolicyList           []
                                  :attachedManagedPolicies  []}})

(defn make-managed-policy-ci
  "Minimal AWS Config CI map for AWS::IAM::Policy."
  [arn account-id policy-doc]
  (let [policy-name (last (str/split arn #"/"))]
    {:arn                          arn
     :accountId                    account-id
     :awsRegion                    "us-east-1"
     :resourceType                 "AWS::IAM::Policy"
     :resourceId                   arn
     :resourceName                 policy-name
     :configurationItemStatus      "OK"
     :configurationItemCaptureTime "2024-01-01T00:00:00Z"
     :configurationStateId         "1"
     :configuration                {:arn               arn
                                    :policyName        policy-name
                                    :policyVersionList [{:isDefaultVersion true
                                                         :document         policy-doc}]}}))

(def gen-role-ci
  (gen/let [arn       gen-role-arn
            acct      gen-account-id
            trust-doc gen-trust-doc-service]
    (make-role-ci arn acct trust-doc)))

(def gen-managed-policy-ci
  (gen/let [arn        gen-policy-arn
            acct       gen-account-id
            policy-doc gen-allow-policy-doc]
    (make-managed-policy-ci arn acct policy-doc)))

;;; ─── Group 1: Round-trip & lifecycle ────────────────────────────────────────

(defspec round-trip-role 50
  (prop/for-all [ci gen-role-ci]
    (with-fresh-db
      (sut/ingest! ci)
      (let [result (sut/get-by-arn (:arn ci))]
        (and (some? result)
             (= (:arn ci) (:ci/arn result)))))))

(defspec round-trip-policy 50
  (prop/for-all [ci gen-managed-policy-ci]
    (with-fresh-db
      (sut/ingest! ci)
      (let [result (sut/get-by-arn (:arn ci))]
        (and (some? result)
             (= (:arn ci) (:ci/arn result)))))))

(defspec ingest-is-idempotent 50
  (prop/for-all [ci gen-role-ci]
    (with-fresh-db
      (sut/ingest! ci)
      (sut/ingest! ci)
      (some? (sut/get-by-arn (:arn ci))))))

(defspec delete-removes-entity 50
  (prop/for-all [ci gen-role-ci]
    (with-fresh-db
      (sut/ingest! ci)
      (sut/delete-by-arn! (:arn ci))
      (nil? (sut/get-by-arn (:arn ci))))))

;;; ─── Group 2: Query consistency ─────────────────────────────────────────────

(defspec role-appears-in-list-roles 50
  (prop/for-all [ci gen-role-ci]
    (with-fresh-db
      (sut/ingest! ci)
      (contains? (set (map :arn (sut/list-roles))) (:arn ci)))))

(defspec account-filter-is-exact 50
  ;; Insert CIs in two distinct accounts; querying one must not return the other.
  (prop/for-all [[arn-a arn-b]   gen-two-role-arns
                 [acct-a acct-b] gen-two-account-ids]
    (with-fresh-db
      (sut/ingest! (make-role-ci arn-a acct-a (trust-doc-for-service "lambda.amazonaws.com")))
      (sut/ingest! (make-role-ci arn-b acct-b (trust-doc-for-service "ec2.amazonaws.com")))
      (let [arns-for-a (set (map :arn (sut/list-by-account acct-a)))]
        (and (contains? arns-for-a arn-a)
             (not (contains? arns-for-a arn-b)))))))

(defspec service-trust-query 50
  (prop/for-all [svc  gen-service
                 arn  gen-role-arn
                 acct gen-account-id]
    (with-fresh-db
      (sut/ingest! (make-role-ci arn acct (trust-doc-for-service svc)))
      (contains? (set (map :arn (sut/roles-assuming-service svc))) arn))))

(defspec allowed-actions-only-allows 50
  (prop/for-all [ci   gen-role-ci
                 pdoc gen-deny-policy-doc]
    (with-fresh-db
      (let [ci-deny (assoc-in ci [:configuration :rolePolicyList]
                              [{:policyName "DenyAll" :policyDocument pdoc}])]
        (sut/ingest! ci-deny)
        (empty? (sut/role-allowed-actions (:arn ci)))))))

;;; ─── Group 3: Trust graph invariants ────────────────────────────────────────

(defspec trust-symmetry 50
  (prop/for-all [[arn-a arn-b] gen-two-role-arns
                 acct          gen-account-id]
    (with-fresh-db
      (let [ci-a (make-role-ci arn-a acct (trust-doc-for-service "lambda.amazonaws.com"))
            ci-b (make-role-ci arn-b acct (trust-doc-for-role arn-a))]
        (sut/ingest-many! [ci-a ci-b])
        (and (contains? (set (map :arn (sut/roles-trusting arn-a))) arn-b)
             (contains? (set (map :arn (sut/roles-trusted-by arn-b))) arn-a))))))

(defspec trust-graph-completeness 50
  (prop/for-all [[arn-a arn-b] gen-two-role-arns
                 acct          gen-account-id]
    (with-fresh-db
      (let [ci-a (make-role-ci arn-a acct (trust-doc-for-service "ec2.amazonaws.com"))
            ci-b (make-role-ci arn-b acct (trust-doc-for-role arn-a))]
        (sut/ingest-many! [ci-a ci-b])
        ;; roles-trusting(A) returns the roles A can assume (the targets).
        ;; Each such target must appear as :target-arn in the full trust-graph.
        (let [graph-targets    (set (map :target-arn (sut/trust-graph)))
              trusting-targets (set (map :arn (sut/roles-trusting arn-a)))]
          (every? #(contains? graph-targets %) trusting-targets))))))

(defspec ingest-many-order-invariant 50
  (prop/for-all [[arn-a arn-b] gen-two-role-arns
                 acct          gen-account-id]
    (let [ci-a      (make-role-ci arn-a acct (trust-doc-for-service "lambda.amazonaws.com"))
          ci-b      (make-role-ci arn-b acct (trust-doc-for-role arn-a))
          edge-keys (juxt :assumer-arn :target-arn)
          graph-fwd (with-fresh-db
                      (sut/ingest-many! [ci-a ci-b])
                      (set (map edge-keys (sut/trust-graph))))
          graph-rev (with-fresh-db
                      (sut/ingest-many! [ci-b ci-a])
                      (set (map edge-keys (sut/trust-graph))))]
      (= graph-fwd graph-rev))))

;;; ─── Group 4: Policy graph invariants ───────────────────────────────────────

(defspec attached-policy-in-attachments 50
  (prop/for-all [role-arn   gen-role-arn
                 policy-arn gen-policy-arn
                 acct       gen-account-id]
    (with-fresh-db
      (let [policy-name (last (str/split policy-arn #"/"))
            ci-role     (assoc-in (make-role-ci role-arn acct
                                                (trust-doc-for-service "ec2.amazonaws.com"))
                                  [:configuration :attachedManagedPolicies]
                                  [{:policyArn policy-arn :policyName policy-name}])]
        (sut/ingest! ci-role)
        (boolean (some #(and (= (:policy-arn %) policy-arn)
                             (= (:role-arn %) role-arn))
                       (sut/policy-attachments)))))))

(defspec policies-by-action-finds-matching 50
  (prop/for-all [prefix     (gen/such-that #(>= (count %) 2)
                                           (gen/resize 8 gen/string-alphanumeric)
                                           100)
                 policy-arn gen-policy-arn
                 acct       gen-account-id]
    (with-fresh-db
      (let [action (str prefix ":Action")
            doc    (json/generate-string
                    {"Version"   "2012-10-17"
                     "Statement" [{"Effect"   "Allow"
                                   "Action"   [action]
                                   "Resource" "*"}]})
            ci     (make-managed-policy-ci policy-arn acct doc)]
        (sut/ingest! ci)
        (boolean (some #(= (:arn %) policy-arn)
                       (sut/policies-by-action (str prefix ":"))))))))

;;; ─── Group 5: Servicespec (action validation + wildcard queries) ───────────
;;; Uses a single shared DB to avoid LMDB segfault from rapid temp DB churn.

(def ^:private specs-dir "/Users/duke/dev/SPA/aws/iam/servicespec/")

(def ^:private specs-conn
  (let [path (str "/tmp/bb-iam-specs-" (random-uuid))
        c    (d/get-conn path sut/schema)]
    (with-redefs [sut/conn c]
      (sut/load-servicespec! (str specs-dir "s3.json") "s3")
      (sut/load-servicespec! (str specs-dir "iam.json") "iam")
      (sut/load-servicespec! (str specs-dir "lambda.json") "lambda")
      (sut/load-servicespec! (str specs-dir "kms.json") "kms")
      (sut/load-servicespec! (str specs-dir "secretsmanager.json") "secretsmanager")
      (sut/load-servicespec! (str specs-dir "ec2.json") "ec2")
      (sut/load-servicespec! (str specs-dir "sns.json") "sns")
      (sut/load-servicespec! (str specs-dir "sqs.json") "sqs"))
    c))

(defmacro with-specs-db
  "Run body against the shared servicespec DB (read-only tests)."
  [& body]
  `(with-redefs [sut/conn specs-conn]
     ~@body))

(deftest valid-action-recognises-real-actions
  (with-specs-db
    (is (sut/valid-action? "s3:GetObject"))
    (is (sut/valid-action? "iam:CreateRole"))
    (is (sut/valid-action? "lambda:InvokeFunction"))
    (is (sut/valid-action? "*"))
    (is (sut/valid-action? "s3:*"))
    (is (not (sut/valid-action? "s3:MadeUpAction")))
    (is (not (sut/valid-action? "bogus:Nope")))))

(deftest invalid-actions-filters-bad-names
  (with-specs-db
    (let [bad (sut/invalid-actions ["s3:GetObject" "s3:FakeAction" "iam:CreateRole" "zzz:Nope"])]
      (is (= #{"s3:FakeAction" "zzz:Nope"} (set bad))))))

(deftest expand-action-pattern-verb-level
  (with-specs-db
    (let [gets (sut/expand-action-pattern "s3:Get*")]
      (is (> (count gets) 10))
      (is (every? #(str/starts-with? % "s3:Get") gets)))))

(deftest expand-action-pattern-service-wildcard
  (with-specs-db
    (let [all-iam (sut/expand-action-pattern "iam:*")]
      (is (> (count all-iam) 100))
      (is (every? #(str/starts-with? % "iam:") all-iam)))))

(deftest expand-action-pattern-sub-verb-prefix
  (with-specs-db
    (let [matches (sut/expand-action-pattern "s3:GetBucket*")]
      (is (> (count matches) 3))
      (is (every? #(str/starts-with? % "s3:GetBucket") matches)))))

(deftest action-pattern-readonly-check
  (with-specs-db
    (is (true? (sut/action-pattern-readonly? "s3:Get*")))
    (is (true? (sut/action-pattern-readonly? "iam:List*")))
    (is (false? (sut/action-pattern-readonly? "s3:Put*")))
    (is (false? (sut/action-pattern-readonly? "iam:Create*")))
    (is (false? (sut/action-pattern-readonly? "iam:*")))))

(deftest compress-actions-groups-by-access-level
  (with-specs-db
    (let [actions  ["s3:GetObject" "s3:GetBucketAcl" "s3:GetBucketPolicy"
                    "s3:PutObject" "s3:PutObjectTagging"
                    "s3:PutBucketPolicy" "s3:PutBucketAcl"
                    "iam:CreateRole"]
          result   (sut/compress-actions actions)]
      ;; 3 s3:Get* actions all Read → compressed
      (is (contains? (set (get result "Read")) "s3:Get*"))
      ;; s3:PutObject + s3:PutObjectTagging both Write (Tagging merged)
      ;; Not compressed to s3:Put* because PutBucketPolicy/Acl are PermMgmt
      (let [write-set (set (get result "Write"))]
        (is (contains? write-set "s3:PutObject"))
        (is (contains? write-set "s3:PutObjectTagging")))
      ;; s3:PutBucketPolicy + s3:PutBucketAcl are PermissionManagement
      (let [pm (set (get result "PermissionManagement"))]
        (is (contains? pm "s3:PutBucketPolicy"))
        (is (contains? pm "s3:PutBucketAcl"))
        (is (not (contains? pm "s3:Put*"))))
      ;; No Tagging key — merged into Write
      (is (nil? (get result "Tagging"))))))

(deftest valid-resource-for-action-s3
  (with-specs-db
    (is (sut/valid-resource-for-action? "s3:GetObject"
          "arn:aws:s3:::my-bucket/key"))
    (is (sut/valid-resource-for-action? "s3:GetObject" "*"))
    ;; wrong service
    (is (not (sut/valid-resource-for-action? "s3:GetObject"
               "arn:aws:iam::123456789012:role/MyRole")))))

(deftest valid-resource-for-action-iam
  (with-specs-db
    (is (sut/valid-resource-for-action? "iam:CreateRole"
          "arn:aws:iam::123456789012:role/MyRole"))
    ;; IAM has empty region — non-empty region should fail
    (is (not (sut/valid-resource-for-action? "iam:CreateRole"
               "arn:aws:iam:us-east-1:123456789012:role/MyRole")))))

;;; ─── Group 6: expand → compress roundtrips ──────────────────────────────────

(deftest roundtrip-uniform-verb-compresses-back
  ;; expand s3:Get* → individual actions → compress should yield s3:Get* under Read
  (with-specs-db
    (let [expanded  (sut/expand-action-pattern "s3:Get*")
          result    (sut/compress-actions expanded)]
      (is (contains? (set (get result "Read")) "s3:Get*"))
      ;; no individual s3:Get actions should leak out
      (is (not-any? #(and (str/starts-with? % "s3:Get")
                          (not= % "s3:Get*"))
                    (get result "Read"))))))

(deftest roundtrip-uniform-verb-iam-list
  ;; iam:List* → all List → compresses back to iam:List*
  (with-specs-db
    (let [expanded (sut/expand-action-pattern "iam:List*")
          result   (sut/compress-actions expanded)]
      (is (contains? (set (get result "List")) "iam:List*")))))

(deftest roundtrip-mixed-verb-stays-individual
  ;; s3:Put* spans Write + PermMgmt (Tagging merged to Write) → must NOT compress back
  (with-specs-db
    (let [expanded (sut/expand-action-pattern "s3:Put*")
          result   (sut/compress-actions expanded)
          all-vals (into #{} cat (vals result))]
      ;; s3:Put* wildcard must not appear in any access-level group
      (is (not (contains? all-vals "s3:Put*")))
      ;; but all individual actions are still present
      (is (= (count expanded)
             (reduce + (map count (vals result))))))))

(deftest roundtrip-kms-write-all-same-level
  ;; kms crypto ops are all Write — expand + compress should roundtrip
  (with-specs-db
    (let [expanded (sut/expand-action-pattern "kms:Encrypt*")
          result   (sut/compress-actions expanded)]
      ;; only 1 action matches Encrypt* so it stays as singleton
      (is (= 1 (count expanded)))
      (is (contains? (set (get result "Write")) "kms:Encrypt")))))

(deftest roundtrip-service-wildcard-mixed-levels
  ;; iam:* has actions across Read, List, Write, PermMgmt (Tagging merged to Write)
  ;; compress should group by level, compress within uniform verbs
  (with-specs-db
    (let [expanded (sut/expand-action-pattern "iam:*")
          result   (sut/compress-actions expanded)
          all-vals (into [] cat (vals result))]
      ;; every expanded action accounted for — count compressed entries
      ;; (wildcards cover multiple, so total entries <= expanded count)
      (is (<= (count all-vals) (count expanded)))
      ;; must have multiple access levels
      (is (>= (count result) 3))
      ;; iam:List* should compress (all List)
      (is (contains? (set (get result "List")) "iam:List*")))))

(deftest roundtrip-secretsmanager-get-compresses
  ;; secretsmanager:Get* — GetSecretValue + GetResourcePolicy + GetRandomPassword all Read
  (with-specs-db
    (let [expanded (sut/expand-action-pattern "secretsmanager:Get*")
          result   (sut/compress-actions expanded)]
      (is (>= (count expanded) 3))
      (is (contains? (set (get result "Read")) "secretsmanager:Get*")))))

(deftest roundtrip-cross-service-no-merge
  ;; actions from different services with same verb never compress together
  (with-specs-db
    (let [actions ["s3:GetObject" "kms:GetKeyPolicy" "secretsmanager:GetSecretValue"
                   "s3:GetBucketAcl" "kms:GetPublicKey" "secretsmanager:GetResourcePolicy"]
          result  (sut/compress-actions actions)
          reads   (set (get result "Read"))]
      ;; each service compresses independently
      (is (contains? reads "s3:Get*"))
      (is (contains? reads "kms:Get*"))
      (is (contains? reads "secretsmanager:Get*"))
      ;; no cross-service wildcard
      (is (not (contains? reads "Get*"))))))

;;; ─── Group 7: split-statement-by-access-level ───────────────────────────────

(deftest split-statement-basic
  ;; Concrete actions across 3 levels → 3 output statements
  (with-specs-db
    (let [input  (json/generate-string
                   {"Sid"      "MyStmt"
                    "Effect"   "Allow"
                    "Action"   ["s3:GetObject" "s3:GetBucketAcl"
                                "s3:PutObject" "s3:PutBucketPolicy"]
                    "Resource" "*"})
          result (sut/split-statement-by-access-level input)]
      ;; One statement per access level
      (is (= 3 (count result)))
      ;; Each carries Effect and Resource
      (is (every? #(= "Allow" (get % "Effect")) result))
      (is (every? #(= "*" (get % "Resource")) result))
      ;; s3:Get* compressed under Read
      (let [read-stmt (first (filter #(str/ends-with? (get % "Sid") "Read") result))]
        (is (some? read-stmt))
        (is (= ["s3:Get*"] (get read-stmt "Action")))
        (is (= "MyStmtS3Read" (get read-stmt "Sid"))))
      ;; PutObject is Write, PutBucketPolicy is PermissionManagement — not compressed
      (let [write-stmt (first (filter #(str/ends-with? (get % "Sid") "Write") result))]
        (is (= ["s3:PutObject"] (get write-stmt "Action"))))
      (let [pm-stmt (first (filter #(str/includes? (get % "Sid") "Permission") result))]
        (is (= ["s3:PutBucketPolicy"] (get pm-stmt "Action")))))))

(deftest split-statement-with-wildcards
  ;; Wildcard s3:Get* stays as-is, classified under Read
  (with-specs-db
    (let [input  (json/generate-string
                   {"Effect"   "Allow"
                    "Action"   ["s3:Get*" "s3:PutObject"]
                    "Resource" "*"})
          result (sut/split-statement-by-access-level input)]
      (let [read-stmt (first (filter #(str/ends-with? (get % "Sid") "Read") result))]
        (is (some? read-stmt))
        (is (= ["s3:Get*"] (get read-stmt "Action"))))
      (let [write-stmt (first (filter #(str/ends-with? (get % "Sid") "Write") result))]
        (is (= ["s3:PutObject"] (get write-stmt "Action")))))))

(deftest split-statement-star-action
  ;; Bare "*" action → Mixed level
  (with-specs-db
    (let [input  (json/generate-string
                   {"Effect" "Allow" "Action" "*" "Resource" "*"})
          result (sut/split-statement-by-access-level input)]
      (is (= 1 (count result)))
      (is (= ["*"] (get (first result) "Action")))
      (is (str/includes? (get (first result) "Sid") "Mixed")))))

(deftest split-statement-unknown-actions
  ;; Invalid actions collected under Unknown
  (with-specs-db
    (let [input  (json/generate-string
                   {"Effect"   "Allow"
                    "Action"   ["s3:GetObject" "s3:FakeAction"]
                    "Resource" "*"})
          result (sut/split-statement-by-access-level input)
          unk    (first (filter #(str/includes? (get % "Sid") "Unknown") result))]
      (is (some? unk))
      (is (= ["s3:FakeAction"] (get unk "Action"))))))

(deftest split-statement-no-original-sid
  ;; No Sid in input → generated from service + level
  (with-specs-db
    (let [input  (json/generate-string
                   {"Effect"   "Allow"
                    "Action"   ["kms:Decrypt" "kms:DescribeKey"]
                    "Resource" "*"})
          result (sut/split-statement-by-access-level input)
          sids   (set (map #(get % "Sid") result))]
      (is (contains? sids "KmsWrite"))
      (is (contains? sids "KmsRead")))))

(deftest split-statement-multi-service-returns-as-is
  ;; Multiple services in actions → return input unchanged as singleton
  (with-specs-db
    (let [input  (json/generate-string
                   {"Sid"      "Mixed"
                    "Effect"   "Allow"
                    "Action"   ["s3:GetObject" "kms:Decrypt"]
                    "Resource" "*"})
          result (sut/split-statement-by-access-level input)]
      (is (= 1 (count result)))
      (is (= ["s3:GetObject" "kms:Decrypt"] (get (first result) "Action")))))

(deftest split-statement-multi-service-via-resource
  ;; Single service in actions but resource ARN is a different service → as-is
  (with-specs-db
    (let [input  (json/generate-string
                   {"Effect"   "Allow"
                    "Action"   ["s3:GetObject"]
                    "Resource" "arn:aws:kms:us-east-1:123456789012:key/abc"})
          result (sut/split-statement-by-access-level input)]
      (is (= 1 (count result)))
      (is (= ["s3:GetObject"] (get (first result) "Action")))))))

(deftest split-statement-preserves-condition
  ;; Condition and Principal carried through to all output statements
  (with-specs-db
    (let [input  (json/generate-string
                   {"Effect"    "Allow"
                    "Action"    ["s3:GetObject" "s3:PutObject"]
                    "Resource"  "arn:aws:s3:::my-bucket/*"
                    "Condition" {"StringEquals" {"s3:prefix" "home/"}}})
          result (sut/split-statement-by-access-level input)]
      (is (>= (count result) 2))
      (is (every? #(some? (get % "Condition")) result))
      (is (every? #(= "arn:aws:s3:::my-bucket/*" (get % "Resource")) result)))))

;;; ─── Group 8: Full policy document split ─────────────────────────────────────
;;; Realistic managed policy with 7 statements across s3, iam, ec2, kms, sns, sqs.

(def ^:private test-policy-doc
  {"Version" "2012-10-17"
   "Statement"
   [;; Stmt 0: s3 mixed read + write + wildcard
    {"Sid"       "S3Data"
     "Effect"    "Allow"
     "Action"    ["s3:GetObject" "s3:GetBucketAcl" "s3:GetObjectTagging"
                  "s3:PutObject" "s3:PutObjectTagging" "s3:PutBucketPolicy"
                  "s3:GetBucket*"]
     "Resource"  "arn:aws:s3:::my-bucket/*"}

    ;; Stmt 1: iam read + list + tagging
    {"Sid"       "IamAudit"
     "Effect"    "Allow"
     "Action"    ["iam:ListInstanceProfiles" "iam:ListUserTags"
                  "iam:ListPoliciesGrantingServiceAccess"
                  "iam:TagUser"]
     "Resource"  "*"}

    ;; Stmt 2: ec2 mixed write + read
    {"Sid"       "Ec2Ops"
     "Effect"    "Allow"
     "Action"    ["ec2:DescribeSnapshotTierStatus" "ec2:CreatePublicIpv4Pool"
                  "ec2:DeleteCustomerGateway" "ec2:EnableImageBlockPublicAccess"]
     "Resource"  "*"}

    ;; Stmt 3: kms write + list
    {"Sid"       "KmsKeys"
     "Effect"    "Allow"
     "Action"    ["kms:EnableKey" "kms:DisableKey" "kms:EnableKeyRotation"
                  "kms:ListKeys"]
     "Resource"  "arn:aws:kms:us-east-1:123456789012:key/*"
     "Condition" {"StringEquals" {"kms:ViaService" "s3.us-east-1.amazonaws.com"}}}

    ;; Stmt 4: mixed sns+sqs (multi-service → returned as-is)
    {"Sid"       "Messaging"
     "Effect"    "Allow"
     "Action"    ["sns:Publish" "sqs:SendMessage" "sqs:GetQueueAttributes"]
     "Resource"  ["arn:aws:sns:us-east-1:123456789012:my-topic"
                  "arn:aws:sqs:us-east-1:123456789012:my-queue"]}

    ;; Stmt 5: Deny statement — s3 delete
    {"Sid"       "DenyS3Delete"
     "Effect"    "Deny"
     "Action"    ["s3:DeleteObject" "s3:DeleteBucket"]
     "Resource"  "arn:aws:s3:::my-bucket/*"
     "Condition" {"StringNotLike" {"aws:PrincipalTag/team" "admin"}}}

    ;; Stmt 6: NotAction — should return as-is
    {"Sid"       "DenyAllExceptIam"
     "Effect"    "Deny"
     "NotAction" ["iam:ChangePassword" "iam:GetUser"]
     "Resource"  "*"}]})

(deftest policy-split-s3-data-stmt
  ;; Stmt 0: s3 mixed concrete + wildcard → splits by level (Tagging merged to Write)
  (with-specs-db
    (let [stmt   (get-in test-policy-doc ["Statement" 0])
          result (sut/split-statement-by-access-level (json/generate-string stmt))]
      ;; Must produce multiple groups (Read, Write, PermMgmt)
      (is (>= (count result) 3))
      ;; s3:GetBucket* wildcard kept as-is, classified under Read
      (let [read-stmt (first (filter #(str/ends-with? (get % "Sid") "Read") result))]
        (is (some? read-stmt))
        (is (contains? (set (get read-stmt "Action")) "s3:GetBucket*")))
      ;; Resource carried to every output statement
      (is (every? #(= "arn:aws:s3:::my-bucket/*" (get % "Resource")) result))
      ;; Effect is Allow on all
      (is (every? #(= "Allow" (get % "Effect")) result)))))

(deftest policy-split-iam-audit-stmt
  ;; Stmt 1: iam list + tagging(→Write) → 2 groups
  (with-specs-db
    (let [stmt   (get-in test-policy-doc ["Statement" 1])
          result (sut/split-statement-by-access-level (json/generate-string stmt))
          sids   (set (map #(get % "Sid") result))]
      (is (= 2 (count result)))
      ;; iam:List* compressed
      (let [list-stmt (first (filter #(str/ends-with? (get % "Sid") "List") result))]
        (is (some? list-stmt))
        (is (= ["iam:List*"] (get list-stmt "Action"))))
      ;; TagUser merged into Write
      (let [write-stmt (first (filter #(str/ends-with? (get % "Sid") "Write") result))]
        (is (some? write-stmt))
        (is (= ["iam:TagUser"] (get write-stmt "Action")))))))

(deftest policy-split-kms-condition-preserved
  ;; Stmt 3: kms with Condition → Condition copied to each output
  (with-specs-db
    (let [stmt   (get-in test-policy-doc ["Statement" 3])
          result (sut/split-statement-by-access-level (json/generate-string stmt))]
      ;; Write (Enable/Disable/EnableRotation) + List (ListKeys) → 2 groups
      (is (= 2 (count result)))
      ;; Condition on every output
      (is (every? #(= {"StringEquals" {"kms:ViaService" "s3.us-east-1.amazonaws.com"}}
                       (get % "Condition"))
                  result))
      ;; Resource carried through
      (is (every? #(= "arn:aws:kms:us-east-1:123456789012:key/*" (get % "Resource"))
                  result)))))

(deftest policy-split-mixed-sns-sqs-as-is
  ;; Stmt 4: multi-service sns+sqs → returned unchanged
  (with-specs-db
    (let [stmt   (get-in test-policy-doc ["Statement" 4])
          result (sut/split-statement-by-access-level (json/generate-string stmt))]
      (is (= 1 (count result)))
      (is (= (sort (get stmt "Action")) (sort (get (first result) "Action"))))
      (is (= (get stmt "Resource") (get (first result) "Resource"))))))

(deftest policy-split-deny-effect-preserved
  ;; Stmt 5: Deny + Condition → Effect=Deny on all outputs, Condition carried
  (with-specs-db
    (let [stmt   (get-in test-policy-doc ["Statement" 5])
          result (sut/split-statement-by-access-level (json/generate-string stmt))]
      (is (pos? (count result)))
      (is (every? #(= "Deny" (get % "Effect")) result))
      (is (every? #(some? (get % "Condition")) result)))))

(deftest policy-split-not-action-as-is
  ;; Stmt 6: NotAction → returned as-is (no splitting)
  (with-specs-db
    (let [stmt   (get-in test-policy-doc ["Statement" 6])
          result (sut/split-statement-by-access-level (json/generate-string stmt))]
      (is (= 1 (count result)))
      ;; NotAction key preserved
      (is (= ["iam:ChangePassword" "iam:GetUser"]
             (get (first result) "NotAction")))
      (is (= "Deny" (get (first result) "Effect"))))))

(deftest policy-split-not-resource-as-is
  ;; NotResource → returned as-is
  (with-specs-db
    (let [stmt (json/generate-string
                 {"Effect"      "Deny"
                  "Action"      ["s3:DeleteObject"]
                  "NotResource" "arn:aws:s3:::protected-bucket/*"})
          result (sut/split-statement-by-access-level stmt)]
      (is (= 1 (count result)))
      (is (= "arn:aws:s3:::protected-bucket/*"
             (get (first result) "NotResource"))))))

;;; ─── Runner ──────────────────────────────────────────────────────────────────

(defn -main [& _args]
  (run-tests 'iam-oracle-test))
