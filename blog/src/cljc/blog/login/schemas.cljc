(ns blog.login.schemas
  (:require [schema.core :as s]))

(s/defschema login-model {:username s/Str
                          :password s/Str})

(s/defschema login-response {:nickname s/Str
                             :img_location s/Str
                             :primary-group-name s/Str})
