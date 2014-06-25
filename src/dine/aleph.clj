(ns dine.aleph
  (:use [lamina.core]
        [aleph.tcp]))

(defn table-handler [channel client-info]
  (siphon channel channel))

(defn go []
  (start-tcp-server table-handler {:port 1984}))
