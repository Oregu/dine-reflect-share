(ns dine.aleph
  (:use [lamina.core]
        [aleph.tcp]
        [gloss.core]))

(defn table-handler [ch client-info]
  (receive-all ch
    #(enqueue ch (str "You said " %))))

(def ch
  (wait-for-result
    (tcp-client {:host "localhost",
                 :port 1984,
                 :frame (string :utf-8 :delimiters ["\r\n"])})))

(defn dine [myname]
  (do
    (enqueue ch "Hello, server!")
    (wait-for-message ch)))

(defn go []
  (let [folks [:Aristotle :Kant :Spinoza :Marx :Russel]]
    (start-tcp-server table-handler {:port 1984, :frame (string :utf-8 :delimiters ["\r\n"])})
    (map dine folks)))
