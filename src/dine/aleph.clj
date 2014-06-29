(ns dine.aleph
  (:use [lamina.core]
        [aleph.tcp]
        [gloss.core]))

(defn table-handler [ch client-info]
  (receive-all ch
    #(do
      (println "got msg" %)
      (enqueue ch "You can dine now"))))

(defn dine [myname]
  (let [ch (wait-for-result
             (tcp-client {:host "localhost",
                          :port 1984,
                          :frame (string :utf-8 :delimiters ["\r\n"])}))]

    (enqueue ch (str "Hello from " myname))
    (let [msg (read-channel ch)]
      (println "server answered:" msg)
      (close ch))))

(defn go []
  (let [folks [:Aristotle :Kant :Spinoza :Marx :Russel]]
    (let [server (start-tcp-server table-handler
                   {:name "Noodles",
                    :port 1984,
                    :frame (string :utf-8 :delimiters ["\r\n"])})]

      (doall (map dine folks))
      (Thread/sleep 5000)

      (server) ; closing server
      :done)))
