(ns murja.specs.post
  (:require [clojure.spec.alpha :as spec]
            [murja.specs.user :as user])
  (:import [java.time LocalDate]))

(spec/def ::title string?)
(spec/def ::id int?)
(spec/def ::content string?)
(spec/def ::creator ::user/User)
(spec/def ::created_at (partial instance? LocalDate))
(spec/def ::tag string?)
(spec/def ::tags (spec/* ::tag))
(spec/def ::amount-of-comments int?)
(spec/def ::version (spec/nilable int?))
(spec/def ::versions (spec/* ::version))

; (s/optional-key :version) (s/maybe s/Int)}

(spec/def ::Post (spec/keys :req-un [::title ::id ::content ::creator ::created_at ::tags ::amount-of-comments]
                            :opt-un [::version]))
(spec/def ::New-post (spec/keys :req-un [::title ::content ::tags]))

(spec/def ::Edited-post (spec/keys :req-un [::title ::content ::tags ::id]))

#_{:keys [title content tags id]}

(spec/def ::Comment (spec/keys :req-un [::content ::creator ::created_at ::id]))
(spec/def ::parent-post-id int?)
(spec/def ::New-comment (spec/keys :req-un [::content ::parent-post-id]))
