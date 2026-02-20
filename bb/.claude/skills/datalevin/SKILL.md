---
name: datalevin
description: "Datalevin reference - embedded Datalog database with ACID semantics."
---

# Datalevin Skill Reference

**Type:** Embedded Datalog database (think "Datalog-powered SQLite")
**Deployment:** Babashka pod (no JVM overhead), embedded library, or server mode
**Version:** 0.10.5 (Feb 2026)

## Design Philosophy

- ✅ **Conventional ACID semantics** - behaves like a normal database
- ✅ **Schema-optional** with declared special attributes
- ✅ **Novel cost-based query optimizer** - 2.4x faster than PostgreSQL
- ✅ **Lightning-fast recursive rules** - orders of magnitude faster than Datomic
- ❌ **No temporal history** - deleted data is gone (this is intentional)
- ❌ **No time travel** - no `as-of`/`since`/`history`

**Mental model:** "Datalog-powered SQLite" NOT "lightweight Datomic"

---

## Babashka Pod Setup

```clojure
;; In bb.edn
{:pods {datalevin/datalevin {:version "0.10.5"}}}
```

### Require in .cljc files (reader conditional)

The pod namespace is `pod.huahaiy.datalevin`, NOT `datalevin.core`.
Use reader conditionals in `.cljc` files to support both bb and JVM:

```clojure
(ns my-app
  (:require
   #?(:bb  [pod.huahaiy.datalevin :as d]
      :clj [datalevin.core :as d])))
```

**CRITICAL: `:bb` must come BEFORE `:clj`!** Babashka supports BOTH `:clj` and `:bb` reader features — the first matching branch wins. If `:clj` comes first, bb picks `datalevin.core` (not found in pod) and fails with `FileNotFoundException`.

### clj-kondo config for .cljc

kondo analyzes `.cljc` for both `:clj` and `:cljs` branches. The `:cljs` branch has no datalevin require, causing false positives. Suppress in `.clj-kondo/config.edn`:

```edn
{:config-in-ns
 {my-app
  {:linters {:unresolved-namespace {:exclude [d]}
             :unresolved-symbol {:exclude [slurp]}}}}}
```

### Plain .clj files under bb

For `.clj` files that only run under bb, require directly:

```clojure
(require '[pod.huahaiy.datalevin :as d])
```

If a `.clj` file needs to work in both bb and JVM (e.g. test files), rename it to `.cljc` and use the reader conditional pattern above.

---

## dtlv CLI Tool

**Use this when exploring design and debugging!**

```bash
# Interactive REPL
dtlv help

user> (help)
# Functions available: datalevin.core, datalevin.interpret, datalevin.client
# Call without namespace: (<function name> <arguments>)
# Read docs: (doc <function name>)

# Inspect database
dtlv exec /path/to/db "(d/q '[:find ?e :where [?e _ _]] (d/db))"
```

---

## Core API

### Connection & Lifecycle

```clojure
(require '[datalevin.core :as d])

;; Define schema (map of maps, not vector!)
(def schema
  {:todo/name {:db/valueType :db.type/string
               :db/cardinality :db.cardinality/one}
   :todo/tags {:db/cardinality :db.cardinality/many}
   :user/email {:db/unique :db.unique/identity}})

;; Connect (creates if doesn't exist)
(def conn (d/get-conn "/path/to/db" schema))

;; Get immutable snapshot for queries
(def db (d/db conn))

;; Close when done
(d/close conn)
```

**Key points:**

- Schema is **not transacted** - passed at connection time
- Schema uses `:db/cardinality`, `:db/valueType`, `:db/unique` like Datomic
- Connection is local path (embedded) or `dtlv://user:pass@host/dbname` (server)

---

### Transactions

```clojure
;; Create — use identity attributes, NOT negative temp IDs (see gotchas)
(d/transact! conn
  [{:todo/id (str (random-uuid)) :todo/name "Buy milk" :todo/done false}
   {:todo/id (str (random-uuid)) :todo/name "Walk dog" :todo/done false}])

;; Update with :db/add
(d/transact! conn
  [[:db/add entity-id :todo/done true]])

;; Upsert via identity attribute (lookup ref)
(d/transact! conn
  [{:todo/id "existing-uuid" :todo/name "Updated name"}])

;; Delete entire entity
(d/transact! conn
  [[:db/retractEntity entity-id]])

;; Async transactions (2.5x+ throughput for write-heavy workloads)
(d/transact-async! conn [{:todo/name "Async task"}])
```

**Return value:** Transaction metadata with resolved entity IDs

---

### Queries (Datalog)

```clojure
;; Basic pattern matching
(d/q '[:find ?name
       :where [?e :todo/name ?name]]
     (d/db conn))
;; => #{["Buy milk"] ["Walk dog"]}

;; With parameters
(d/q '[:find ?e
       :in $ ?name
       :where [?e :todo/name ?name]]
     (d/db conn)
     "Buy milk")

;; Find single result
(d/q '[:find ?e .
       :where [?e :todo/name "Buy milk"]]
     (d/db conn))
;; => 1

;; Find all as vector
(d/q '[:find [?e ...]
       :where [?e :todo/name _]]
     (d/db conn))
;; => [1 2 3]

;; Aggregation
(d/q '[:find (count ?e)
       :where [?e :todo/done false]]
     (d/db conn))
;; => 5

;; Order & limit (v0.9.12+)
(d/q '{:find [?name]
       :where [[?e :todo/name ?name]]
       :order-by [[?name :desc]]
       :limit 10}
     (d/db conn))
```

---

### Pull API

```clojure
;; Pull specific attributes
(d/pull (d/db conn) [:db/id :todo/name :todo/done] entity-id)
;; => {:db/id 1 :todo/name "Buy milk" :todo/done false}

;; Pull all attributes
(d/pull (d/db conn) [*] entity-id)

;; Nested pull (for references)
(d/pull db [:user/name {:user/friends [:db/id :user/name]}] user-id)

;; Batch pull
(d/pull-many (d/db conn) [:db/id :todo/name] [1 2 3])
```

**Note:** Pull always returns current state - no temporal variants exist

---

### Datalog Rules (FAST!)

Rules package reusable `:where` sub-clauses into named patterns. Two rule heads with the same name = logical OR.

```clojure
(def rules
  '[;; Simple rule — reusable filter
    [(service-actions ?svc ?e)
     [?e :action/service ?svc]]

    ;; Two heads = OR (matches "Read" OR "List")
    [(readonly-action ?e)
     [?e :action/access-level "Read"]]
    [(readonly-action ?e)
     [?e :action/access-level "List"]]

    ;; Multi-clause rule
    [(verb-actions ?svc ?verb ?e)
     [?e :action/service ?svc]
     [?e :action/verb ?verb]]

    ;; Recursive rule (trust graph traversal)
    [(reachable ?a ?b)
     [?a :role/trusts ?b]]
    [(reachable ?a ?b)
     [?a :role/trusts ?mid]
     (reachable ?mid ?b)]])

;; Usage — pass rules as `%` in `:in`
(d/q '[:find [?id ...]
       :in $ %
       :where
       (verb-actions "s3" "Get" ?e)
       (readonly-action ?e)
       [?e :action/id ?id]]
     db rules)
```

**Rules are pure Datalog patterns — fully pod-safe.** No Clojure code involved, serialized cleanly to the pod, evaluated entirely by Datalevin's rule engine.

**Performance:** Semi-naive fix-point + magic-set rewrite — orders of magnitude faster than Datomic/Datascript on recursive queries.

---

### Predicate & Binding Expressions in Queries

Clojure functions can be called inside `:where` clauses. Datalevin resolves symbols via: built-in fns > Java methods > `clojure/resolve` > pod-fns.

```clojure
;; Predicate — filters rows (returns truthy/falsy)
(d/q '[:find [?id ...]
       :in $ ?svc ?prefix
       :where
       [?e :action/service ?svc]
       [?e :action/name ?name]
       [(clojure.string/starts-with? ?name ?prefix)]
       [?e :action/id ?id]]
     db "s3" "GetObj")

;; Binding — assigns result to a new variable
(d/q '[:find ?wildcard
       :where
       [?e :action/service ?svc]
       [(str ?svc ":*") ?wildcard]]
     db)

;; Comparison built-ins (no namespace needed)
(d/q '[:find ?name
       :where
       [?e :action/score ?s]
       [(> ?s 50)]
       [?e :action/name ?name]]
     db)
```

**Pod rules for predicates:**

| What | Works in pod? | Example |
|------|--------------|---------|
| Standard lib (full ns) | YES | `[(clojure.string/starts-with? ?n ?p)]` |
| Namespace aliases | NO | `[(str/starts-with? ?n ?p)]` — alias won't resolve |
| `clojure.core` fns | YES | `[(str "a" ?b) ?c]`, `[(> ?x 5)]` |
| User-defined bb fns | NO | Pod runs in separate process, can't see bb fns |
| Comparison built-ins | YES | `[(< ?a ?b)]`, `[(= ?a "val")]` |

**When predicates won't work:** Use `pull` / `d/q` to get data, then filter in Clojure:
```clojure
;; e.g. every? has no Datalog aggregator — must be Clojure-side
(->> (d/q '[:find ?id ?level :where ...] db)
     (every? (fn [[_ level]] (#{"Read" "List"} level))))
```

---

## Extended Features

### Vector Search (AI/ML workloads)

```clojure
;; Add vectors
(add-vec db-conn {:id "doc1" :embedding [0.1 0.2 0.3]})

;; Similarity search
(search-vec db-conn [0.1 0.2 0.3] {:limit 10})
```

### Full-Text Search

```clojure
(new-search-engine {:index-position? true})
(add-doc search-engine doc-ref "searchable text content")
(search search-engine "query terms" {:display [:refs :text]})
```

### Key-Value Mode

```clojure
(def kv-db (d/open-kv "/path/to/kvdb"))
(d/transact-kv kv-db [[:put "table" :key "value"]])
(d/get-value kv-db "table" :key)
```

---

## Datomic Migration Guide

**What's the same:**

- Datalog query syntax identical
- Pull API syntax identical
- Transaction format (entity maps, temp IDs)
- Schema attribute options (`:db/cardinality`, `:db/valueType`, `:db/unique`)

**What's different:**

| Datomic                      | Datalevin                   | Impact                      |
| ---------------------------- | --------------------------- | --------------------------- |
| `as-of`, `since`, `history`  | ❌ None                     | No time travel queries      |
| Schema transacted            | Schema passed at connection | Different initialization    |
| Peer/transactor architecture | Embedded library            | Simpler deployment          |
| Statistical query planner    | Cost-based optimizer        | 2-4x faster queries         |
| Slow recursive rules         | Fast recursive rules        | Orders of magnitude speedup |

---

## Common Gotchas & Solutions

| Issue | Solution |
|-------|----------|
| **Schema format** | Use map of maps, NOT vector of maps |
| **No temporal queries** | No `as-of`/`since` — design for current state only |
| **Deleted data is gone** | No audit trail/undo — add soft-delete flags if needed |
| **Synchronous by default** | Use `transact-async!` for write-heavy workloads |
| **Entity ID portability** | IDs stable within DB, not across databases |

### Babashka Pod-Specific Gotchas

These apply ONLY when using Datalevin as a Babashka pod (`huahaiy/datalevin`):

| Issue | Impact | Workaround |
|-------|--------|------------|
| **Boolean index bug** | `[?e :attr false]` matches entities where attr is `true` | Use string values instead of booleans (e.g. `"Read"` not `false`), or `pull` + Clojure `filter` |
| **Negative temp ID crash** | `{:db/id -1 ...}` crashes pod with `TransientArrayMap` error | Use `:db.unique/identity` + lookup refs instead of temp IDs |
| **Namespace aliases in predicates** | `[(str/starts-with? ?n ?p)]` fails — alias not resolved | Always use fully qualified: `[(clojure.string/starts-with? ?n ?p)]` |
| **User-defined fns in predicates** | Functions defined in bb script not visible to pod process | Use `pull`/`d/q` then filter in Clojure |
| **Reader conditional order** | `#?(:clj ... :bb ...)` — bb picks `:clj` first (supports both features) | Always put `:bb` BEFORE `:clj`: `#?(:bb [...] :clj [...])` |
| **LMDB SIGSEGV on cleanup** | `libdtlv.dylib` crash after rapid temp DB creation/destruction in tests | Benign — tests pass before crash. Known LMDB issue |

**Design principle:** When a boolean attribute matters for queries, prefer a string enum to avoid the boolean index bug. Example:
```clojure
;; BAD — pod boolean bug
:action/is-write {:db/valueType :db.type/boolean}  ; [?e :action/is-write false] matches true!

;; GOOD — string enum, pod-safe
:action/access-level {:db/valueType :db.type/string} ; "Read"|"Write"|"List"|"Tagging"|"PermissionManagement"
```

---

## Performance Tips

✅ **Use recursive rules confidently** - optimizer handles them exceptionally well
✅ **Batch transactions** - 100K+ datoms in <1s
✅ **Enable async mode** - 2.5x+ throughput for writes
✅ **Declare `:db/unique`** - enforces constraints efficiently
✅ **Complex queries are cheap** - cost-based optimizer counts directly

---

## Resources

- **Docs:** <https://cljdoc.org/d/datalevin/datalevin/>
- **GitHub:** <https://github.com/datalevin/datalevin>
- **Query Guide:** <https://github.com/datalevin/datalevin/blob/master/doc/query.md>
- **Rules Guide:** <https://github.com/datalevin/datalevin/blob/master/doc/rules.md>
- **Changelog:** <https://github.com/datalevin/datalevin/blob/master/CHANGELOG.md>

---

## Quick Reference

```clojure
;; Require (.cljc — bb + JVM)
#?(:bb  [pod.huahaiy.datalevin :as d]
   :clj [datalevin.core :as d])

;; Setup
(def conn (d/get-conn "/db/path" schema))
(def db (d/db conn))

;; CRUD (pod-safe — no negative temp IDs)
(d/transact! conn [{:item/id "uuid" :item/name "value"}]) ; Create (identity attr)
(d/pull db [:item/name] [:item/id "uuid"])                 ; Read (lookup ref)
(d/transact! conn [{:item/id "uuid" :item/name "new"}])   ; Upsert
(d/transact! conn [[:db/retractEntity [:item/id "uuid"]]]) ; Delete

;; Query with rules
(d/q '[:find [?id ...] :in $ % :where (my-rule ?e) [?e :item/id ?id]]
     db my-rules)

;; Predicate (fully qualified ns only in pod!)
(d/q '[:find ?name :where [?e :item/name ?name]
       [(clojure.string/starts-with? ?name "prefix")]]
     db)

;; Close
(d/close conn)
```
