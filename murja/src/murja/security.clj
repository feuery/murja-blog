(ns murja.security)

(defn sha-512 [data]
  (let [md (. java.security.MessageDigest getInstance "sha-512")]
    (. md update (.getBytes data))
    (let [bytes (. md digest)]
      (reduce #(str %1 (format "%02x" %2)) "" bytes))))
