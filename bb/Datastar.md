# Clojure Datastar Tutorial (2026)

## What is Datastar?

**Datastar** is a lightweight (11.08 KiB) hypermedia framework that combines:

- **Backend reactivity** (like HTMX) - server drives UI changes
- **Frontend reactivity** (like Alpine.js) - declarative client-side interactivity
- **Real-time updates** via Server-Sent Events (SSE)

**Latest Version:** v1.0.0-RC.7 (December 2025)

It's built by Star Federation (nonprofit, no VC backing) and has official SDKs for 12+ languages including Clojure.

---

## Installation

**Frontend (CDN):**

```html
<script
  type="module"
  src="https://cdn.jsdelivr.net/gh/starfederation/[email protected]/bundles/datastar.js"
></script>
```

**Backend (Clojure deps.edn):**

```clojure
{:deps
 {dev.data-star.clojure/sdk {:mvn/version "RELEASE"}
  dev.data-star.clojure/http-kit {:mvn/version "RELEASE"}}}
```

---

## Core Concepts

### 1. **Signals** (Reactive State)

Signals are reactive variables declared with `data-signals:`:

```html
<div data-signals:count="0" data-signals:name="'Alice'">
  <p>Count: $count</p>
  <p>Hello, $name!</p>
</div>
```

**Convention:** Use `$` prefix for signals (`$count` not `count`).

### 2. **Backend Actions**

Trigger SSE requests with `@` prefix:

```html
<button data-on:click="@get('/hello-sse')">Start</button>
<button data-on:click="@post('/api/save')">Save</button>
```

**Available actions:** `@get()`, `@post()`, `@put()`, `@patch()`, `@delete()`

**Options (v1.0.0-RC.7):**

- `retry: 'auto'` - automatic retry on errors (default)
- `payload: {}` - override fetch payload
- `requestCancellation: 'auto'` - cancel requests when element removed

### 3. **Data Attributes Reference**

| Attribute            | Purpose                | Example                                          |
| -------------------- | ---------------------- | ------------------------------------------------ |
| `data-signals:`      | Declare reactive state | `data-signals:delay="400"`                       |
| `data-bind:`         | Two-way input binding  | `data-bind:delay`                                |
| `data-on:`           | Event listeners        | `data-on:click="@get('/path')"`                  |
| `data-on-interval:`  | Periodic polling       | `data-on-interval__duration.1s="@get('/clock')"` |
| `data-on-intersect:` | Viewport triggers      | `data-on-intersect="@get('/lazy')"`              |
| `data-class:`        | Dynamic classes        | `data-class-active="$isActive"`                  |
| `data-attr:`         | Dynamic attributes     | `data-attr:disabled="$loading"`                  |

**New modifiers in RC.7:**

- `__exit` - fires when element leaves viewport
- `__threshold` - intersection percentage threshold

---

## Clojure Server-Side API

### Setup SSE Handler

```clojure
(require '[starfederation.datastar.clojure.api :as d*]
         '[starfederation.datastar.clojure.adapter.http-kit2 :as hk])

(defn sse [handler]
  (fn [req]
    (hk/->sse-response req
      {hk/on-open
       (fn [sse]
           (handler req sse)))
       hk/on-close
       (fn [_sse status] (println "Closed:" status))
       hk/on-exception
       (fn [e] (println "Error:" e))})))
```

### Core API Functions

**1. `patch-elements!`** - Replace HTML by ID:

```clojure
(d*/patch-elements! sse
  (html [:div {:id "message"} "Updated!"]))
```

Namespace support for SVG/MathML (RC.7):

```clojure
(d*/patch-elements! sse html-string {:namespace "svg"})
```

**2. `patch-signals!`** - Update client-side state:

```clojure
(d*/patch-signals! sse {:count 42 :name "Bob"})
```

**Note (RC.7):** Signal objects now merge diffs instead of replacing!

**3. `execute-script!`** - Run JavaScript:

```clojure
(d*/execute-script! sse "alert('Hello!')")
(d*/console-log! sse "Debug info")
(d*/console-error! sse "Error message")
```

**4. `redirect!`** - Navigate browser:

```clojure
(d*/redirect! sse "/success")
```

**5. `remove-element!`** - Delete DOM elements:

```clojure
(d*/remove-element! sse "#old-item")
```

**6. `get-signals`** - Read client state:

```clojure
(defn handler [req sse]
  (let [signals (-> req d*/get-signals parse-json)
        delay (:delay signals)]
    ;; Use client-side state on server
    ))
```

**7. `close-sse!`** - Close connection:

```clojure
(d*/close-sse! sse)
```

**8. Helper attributes generators:**

```clojure
(d*/sse-get "/path")    ; Returns "@get('/path')"
(d*/sse-post "/path")   ; Returns "@post('/path')"
```

---

## Common Patterns

### Pattern 1: **One-Shot Update**

Send patch and close:

```clojure
(defn click-handler [req sse]
  (d*/patch-elements! sse
    (html [:h1 {:id "result"} "Clicked!"]))
  (d*/close-sse! sse))
```

### Pattern 2: **Streaming Updates**

Multiple patches with delays:

```clojure
(defn progress-handler [req sse]
  (doseq [i (range 1 11)]
    (d*/patch-elements! sse
      (html [:div {:id "progress"}
             [:progress {:value i :max 10}]]))
    (Thread/sleep 500))
  (d*/close-sse! sse))
```

### Pattern 3: **Long-Lived Connection with Polling**

Keep SSE open, client re-triggers:

```clojure
(defn clock-sse [req sse]
  (d*/patch-elements! sse
    (html [:h1 {:id "clock"
                :data-on-interval__duration.10s (d*/sse-get "/clock-sse")}
           (now)])))
;; Don't call close-sse! - let client re-trigger via interval
```

### Pattern 4: **Broadcast to Multiple Clients**

```clojure
(def connections (atom #{}))

(defn sse-handler [req]
  (hk/->sse-response req
    {hk/on-open (fn [sse] (swap! connections conj sse))
     hk/on-close (fn [sse _] (swap! connections disj sse))}))

(defn broadcast! [html-string]
  (doseq [sse @connections]
    (d*/patch-elements! sse html-string)))

;; Trigger from anywhere:
(broadcast! (html [:div {:id "alert"} "New message!"]))
```

### Pattern 5: **Reading Client Signals**

```clojure
(defn echo-handler [req sse]
  (let [{:keys [message username]} (-> req d*/get-signals parse-json)]
    (d*/patch-elements! sse
      (html [:p {:id "echo"}
             (str username " said: " message)]))))
```

HTML side:

```html
<div data-signals:message="''" data-signals:username="'Guest'">
  <input data-bind:username placeholder="Name" />
  <input data-bind:message placeholder="Message" />
  <button data-on:click="@post('/echo')">Echo</button>
  <p id="echo"></p>
</div>
```

---

## HTML Examples

### Auto-updating Clock (Polling)

```html
<h1 id="clock" data-on-interval__duration.1s="@get('/clock-sse')">
  Loading...
</h1>
```

### Form with Two-Way Binding

```html
<div data-signals:email="''" data-signals:password="''">
  <input data-bind:email type="email" placeholder="Email" />
  <input data-bind:password type="password" />
  <button data-on:click="@post('/login')">Login</button>
  <p>Logging in as: $email</p>
</div>
```

### Lazy Loading on Scroll

```html
<div id="lazy-content" data-on-intersect="@get('/load-more')">
  Loading more...
</div>
```

Exit viewport trigger (RC.7):

```html
<div data-on-intersect__exit="@get('/mark-unseen')">Content</div>
```

### Dynamic Classes

```html
<div data-signals:active="false">
  <button data-on:click="$active = !$active">Toggle</button>
  <div data-class-highlight="$active">Highlighted when active</div>
</div>
```

### Counter Example

```html
<div data-signals:count="0">
  <button data-on:click="$count--">-</button>
  <span>Count: $count</span>
  <button data-on:click="$count++">+</button>
  <button data-on:click="@post('/save-count')">Save</button>
</div>
```

---

## Advanced Features

### Compression (for long-lived SSE)

```clojure
(hk/->sse-response req
  {hk/on-open handler
   hk/write-profile hk/gzip-profile})  ; Enable gzip compression
```

SSE compresses extremely well due to shared compression windows!

### Form Validation

```html
<form data-on:submit="@post('/save')" novalidate>
  <!-- Fields... -->
</form>
```

**Note (RC.7):** `novalidate` now properly skips validation!

### Throttle/Debounce

```html
<!-- Debounce: wait 300ms after last keystroke -->
<input data-on:input__debounce.300ms="@get('/search')" />

<!-- Throttle: at most once per 1s -->
<div data-on:scroll__throttle.1s="@get('/track')"></div>
```

### State Watches for Broadcasting

```clojure
(def !app-state (atom {:users [] :messages []}))

(add-watch !app-state :broadcast
  (fn [_ _ old new]
    (when (not= old new)
      (broadcast! (render-state new)))))

;; Now any state change auto-broadcasts to all clients!
(swap! !app-state update :messages conj {:text "Hello"})
```

---

## Key Features in v1.0.0-RC.7

1. **Signal merging:** Setting `$user` to `{name: "Bob"}` merges with existing object
2. **Retry by default:** Failed requests auto-retry on 4xx/5xx errors
3. **Request cancellation:** Requests auto-cancel when element removed
4. **Namespace patching:** Proper SVG/MathML element support
5. **Intersection improvements:** `__exit` and `__threshold` modifiers
6. **Multiple cleanups:** Attributes can have multiple cleanup functions
7. **Backend action defaults:** GET requests use `openWhenHidden: false`

---

## Complete Example

**Server (core.clj):**

```clojure
(def !messages (atom []))

(defn chat-page [req]
  {:status 200
   :body (render-file "chat.html" {})})

(defn send-message [req sse]
  (let [{:keys [username text]} (-> req d*/get-signals parse-json)
        msg {:username username :text text :time (now)}]
    (swap! !messages conj msg)
    (broadcast-messages!)))

(defn chat-stream [req sse]
  ;; Keep connection open for broadcasts
  (swap! connections conj sse)
  ;; Send current messages
  (d*/patch-elements! sse (render-messages @!messages)))

(defn broadcast-messages! []
  (let [html (render-messages @!messages)]
    (doseq [sse @connections]
      (d*/patch-elements! sse html))))

(def routes
  {"GET /chat" chat-page
   "POST /send" (sse #'send-message)
   "GET /chat-stream" (sse #'chat-stream)})
```

**Client (chat.html):**

```html
<div data-signals:username="'Guest'" data-signals:text="''">
  <input data-bind:username placeholder="Your name" />

  <div id="messages" data-on-load="@get('/chat-stream')">
    <!-- Messages appear here -->
  </div>

  <input data-bind:text placeholder="Type message..." />
  <button data-on:click="@post('/send')">Send</button>
</div>
```

---

## Resources

- **Official Site:** <https://data-star.dev/>
- **Getting Started:** <https://data-star.dev/guide/getting_started>
- **GitHub:** <https://github.com/starfederation/datastar>
- **Clojure SDK:** <https://github.com/starfederation/datastar-clojure>
- **Community Resources:** <https://alvarolm.github.io/datastar-resources/>
- **Latest Release:** <https://github.com/starfederation/datastar/releases/tag/v1.0.0-RC.7>

---

## Tips & Best Practices

1. **Keep it simple:** Start with basic patterns, add complexity only when needed
2. **Use IDs:** Every element you want to patch needs a unique `id`
3. **Signal naming:** Prefix with `$` for clarity
4. **Close connections:** Always call `d*/close-sse!` for one-shot updates
5. **Compression:** Enable gzip for long-lived SSE streams
6. **State watching:** Use atom watches to trigger broadcasts automatically
7. **Error handling:** Leverage RC.7's auto-retry for resilience
8. **Testing:** Use browser DevTools to watch SSE streams (Network tab)

---

## Common Gotchas

- **Missing IDs:** `patch-elements!` requires matching IDs in DOM
- **Forgetting to close:** One-shot handlers should call `close-sse!`
- **Signal syntax:** Use `$signal` not just `signal` in expressions
- **JSON parsing:** Remember to parse signals: `(parse-json (d*/get-signals req))`
- **Hiccup conversion:** Convert hiccup to HTML string: `(str (h/html [...]))`
