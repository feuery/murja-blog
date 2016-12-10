# MURJA-BLOG 

A blogging system that's supposed to fill my needs:

* Posts and comments can be written with html formatting 
* Backend infrastructure supports tree comments, I haven't had the time to make the UI yet
* This is written in Clojure & ClojureScript, so it's trivially expandable
* Backend has a clean-ish REST API so it's trivial to make an emacs-mode that lets the user to publish their html buffers by calling M-x murja-publish-buffer (or whatever the emacs client will be called)
* Running this should be a lot less pain in the arse than Wordpress. 

## Why? 
I needed to migrate from blogspot to my own server and didn't want to run Wordpress

## Dev-env
Run `vagrant up` in the root directory. That downloads an arch-linux box and sets up the database. Afterwards you should call blog.ragtime/migrate which sets up the schema. 

## db
blog.ragtime/start-db has the db configuration hardcoded. This config supports the dev-env script. 

## TODO
The most important things to fix are to make start-db read it's config from a config file and to make the provisioning of the dev-env-box use ansible or something like that instead of stateful non-idempotent shell-scripts. 
