# MURJA-BLOG 

A blogging system that's supposed to fill my needs:

* Posts can be written with html formatting 
* Written in Clojure & Elm
* Running this should be a lot less pain in the arse than Wordpress. 

And the most important factor, if it worked: 
* Posts can be written inside Emacs

but because the emacs-mode is a bit broken, and I can't be arsed to fix it, there's an [ace-editor component](https://www.npmjs.com/package/ace-custom-element) running in the web UI that's configured to behave as like emacs as possible.

## Why? 
I needed to migrate from blogspot to my own server and didn't want to run Wordpress. Also, Clojure and Emacs are cool

## Dev-env
Run `docker-compose start` in the root directory. That starts a postgresql listening to port localhost:5432. Afterwards you should call blog.ragtime/migrate which sets up the schema. 

## Installing
environment/provision.sh demonstrates how this app is set up in [feuerx.net](https://feuerx.net). Buildscript environment/build.sh creates a jar you can set up however you like without fucking with docker images. Configure murja by running `cp ./config.demo.edn /etc/murja/config.edn` and changing the values according to your needs.

## db
blog.ragtime/start-db loads the database configuration from /etc/murja/config.edn. Set the connection details there with this [example](https://github.com/feuery/murja-blog/blob/master/config.demo.edn)
