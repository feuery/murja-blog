# MURJA-BLOG 

A blogging system that's supposed to fill my needs:

* Posts and comments can be written with html formatting 
* Backend infrastructure supports tree comments, I haven't had the time to make the UI yet
* This is written in Clojure & ClojureScript, so it's trivially expandable
* Running this should be a lot less pain in the arse than Wordpress. 

And the most important factor:

* I can publish and edit posts of a murja blog directly from Emacs

## Why? 
I needed to migrate from blogspot to my own server and didn't want to run Wordpress. Also, Clojure and Emacs are cool

## Dev-env
Run `vagrant up` in the root directory. That downloads an arch-linux box and sets up the database. Afterwards you should call blog.ragtime/migrate which sets up the schema. 

## Installing
Follow instructions on the [blog](https://feuerx.net/blog/post/291#installing_murja)

## db
blog.ragtime/start-db loads the database configuration from /etc/murja/config.edn. Set the connection details there with this [example](https://github.com/feuery/murja-blog/blob/master/config.demo.edn)
