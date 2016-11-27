(ns blog.login.schemas
  (:require [schema.core :as s]))

(s/defschema login-model {:username s/Str
                          :password s/Str})

;; This is a moronic replacement for s/either
(s/defschema Permission (s/conditional string? s/Str
                                       keyword? s/Keyword))

(s/defschema login-response {:nickname s/Str
                             :img_location s/Str
                             :userid s/Num
                             :primary-group-name s/Str
                             :permissions [Permission]})
