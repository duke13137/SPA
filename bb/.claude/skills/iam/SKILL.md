---
name: iam
description: "IAM Oracle — query AWS IAM trust graphs, split policy statements by access level, validate actions. DB is pre-loaded."
---

# IAM Oracle

Datalevin-backed IAM knowledge graph. The database is already populated with servicespecs and AWS Config CIs. All commands are `bb -x` and run from the `bb/` directory. Output is JSON.

## tidy-policy — split statement by access level

```bash
# From JSON string
echo '{"Effect":"Allow","Action":["s3:GetObject","s3:PutObject","s3:DeleteObject","s3:ListBucket"],"Resource":"*"}' \
  | bb -x iam-oracle/tidy-policy

# From a file
cat statement.json | bb -x iam-oracle/tidy-policy

# Pipe from jq
jq '.Statement[0]' policy.json | bb -x iam-oracle/tidy-policy
```

Output: JSON array of statements grouped by access level with auto-generated `Sid` like `S3Read`, `S3Write`.

Handles: wildcard expansion, unknown/misspelled actions (→ `Unknown` group), Condition/Principal/Resource carried through. Multi-service or NotAction/NotResource statements returned as-is.

## load-fact — ingest CI

```bash
cat role.json | bb -x iam-oracle/load-fact
jq -c . *.json | bb -x iam-oracle/load-fact    # JSONL
```

## Trust graph

```bash
# Roles that trust this role (it can assume them)
bb -x iam-oracle/who-trusts --arn arn:aws:iam::123456789012:role/MyRole

# Roles trusted by this role (they can assume it)
bb -x iam-oracle/trusted-by --arn arn:aws:iam::123456789012:role/MyRole

# Full role→role trust graph
bb -x iam-oracle/graph

# Transitive trust chain (BFS with depth)
bb -x iam-oracle/chain --arn arn:aws:iam::123456789012:role/MyRole

# Roles assumable by a service
bb -x iam-oracle/by-service --service lambda.amazonaws.com
```

## Policies and actions

```bash
# All policies on a role (inline + attached + trust)
bb -x iam-oracle/policies --arn arn:aws:iam::123456789012:role/MyRole

# All Allow actions for a role
bb -x iam-oracle/actions --arn arn:aws:iam::123456789012:role/MyRole

# Roles with a specific managed policy attached
bb -x iam-oracle/with-policy --arn arn:aws:iam::123456789012:policy/ReadOnly

# All managed-policy↔role attachment pairs
bb -x iam-oracle/attachments

# Managed policies allowing actions matching a prefix
bb -x iam-oracle/find-policies --prefix s3:
```

## Action validation

```bash
# Which actions in a list are invalid? (reads JSON array from stdin)
echo '["s3:GetObject","s3:GettObject","ec2:FakeAction"]' | bb -x iam-oracle/check-actions

# Expand wildcard pattern to matching action IDs
bb -x iam-oracle/expand --pattern 's3:Get*'
bb -x iam-oracle/expand --pattern 'lambda:*'

# Is a wildcard pattern read-only?
bb -x iam-oracle/is-readonly --pattern 's3:Get*'     # true
bb -x iam-oracle/is-readonly --pattern 's3:*'         # false

# Is a resource ARN valid for an action?
bb -x iam-oracle/check-resource --action s3:GetObject --resource 'arn:aws:s3:::my-bucket/*'

# Compress actions by access level (reads JSON array from stdin)
echo '["s3:GetObject","s3:GetBucketAcl","s3:PutObject"]' | bb -x iam-oracle/compress
# => {"Read": ["s3:Get*"], "Write": ["s3:PutObject"]}
```

## Entity lookup

```bash
# Pull full entity by ARN
bb -x iam-oracle/lookup --arn arn:aws:iam::123456789012:role/MyRole

# List all IAM roles
bb -x iam-oracle/roles

# List roles by account
bb -x iam-oracle/roles --account 123456789012

# List all CIs for an account
bb -x iam-oracle/by-account --account 123456789012
```

## Command reference

| Command | Input | Output |
|---|---|---|
| `tidy-policy` | stdin: statement JSON | Array of split statements |
| `load-fact` | stdin: CI JSON or JSONL | Ingestion confirmation |
| `who-trusts --arn` | role ARN | Roles it can assume (targets) |
| `trusted-by --arn` | role ARN | Roles that can assume it |
| `chain --arn` | role ARN | Transitive reachable roles with depth |
| `graph` | — | All role→role trust edges |
| `by-service --service` | service domain | Roles assumable by service |
| `policies --arn` | role ARN | All policies (inline + attached + trust) |
| `actions --arn` | role ARN | Sorted Allow action strings |
| `with-policy --arn` | policy ARN | Roles with that policy attached |
| `attachments` | — | All policy↔role pairs |
| `find-policies --prefix` | action prefix | Managed policies allowing prefix |
| `check-actions` | stdin: JSON array | Invalid actions only |
| `expand --pattern` | wildcard pattern | Matching action IDs |
| `is-readonly --pattern` | wildcard pattern | true/false/null |
| `check-resource --action --resource` | action + ARN | true/false |
| `compress` | stdin: JSON array | `{level: [actions]}` map |
| `lookup --arn` | ARN | Full entity |
| `roles` | optional `--account` | All roles |
| `by-account --account` | account ID | All CIs for account |

## Environment

| Var | Default | Purpose |
|---|---|---|
| `IAM_ORACLE_DB` | `/tmp/iam-oracle.dtlv` | Datalevin database path |
| `IAM_ORACLE_DATA` | `../aws/iam/` | Base dir for servicespecs and samples |
