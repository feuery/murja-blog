package net.feuerx.plugins

//https://themkat.net/2022/09/24/kotlin_in_emacs_redux.html

import kotlinx.html.*
import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.http.content.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.webjars.*
import io.ktor.server.html.respondHtml
import java.io.File

fun Application.configureRouting() {
    install(Webjars) {
        path = "/webjars" //defaults to /webjars
    }
    routing {
        get("/") {
            call.respondHtml(HttpStatusCode.OK) {
		head {
		    link(type="text/css", href="/static/site.css") {}
		    meta(charset="UTF-8")
		    script(type="module", src="https://unpkg.com/ace-custom-element@latest/dist/index.min.js") {}
		    script(type="", src="static/murja.min.js") {defer=true}
		    script(type="", src="static/murja-helper.js") {defer=true}
		}
		body {
		    div{id="app"}
		}
	    }
	}
        get("/webjars") {
            call.respondText("<script src='/webjars/jquery/jquery.js'></script>", ContentType.Text.Html)
        }
	staticFiles("/static", File("/Users/feuer/Projects/murja-blog/kt-backend/src/main/resources/static"))
    }
}
