var app = Elm.Main.init({
    node: document.getElementById("app")
});
app.ports.alert.subscribe( (prompt) => {
    window.alert(prompt);
});

let creationObserver = null;

let runace = (content) => {

    if(creationObserver) creationObserver.disconnect();

    let editor = ace.edit("editor-post-content");
    editor.setTheme("ace/theme/monokai");
    editor.session.setMode('ace/mode/html');
    editor.setKeyboardHandler("ace/keyboard/emacs");
    editor.session.setValue(content);
    
    editor.on('change', event => {
	let value = editor.getSession().getValue();
	app.ports.aceStateUpdate.send(value);
    });

    const config = { attributes: false, childList: true};
    const callback = function(mutationsList, observer) {
	editor.destroy();
	editor = null;
    };
    const observer = new MutationObserver(callback);
    observer.observe(document.getElementById('editor-post-content'), config);

    console.log("ace should be initiated");
};

app.ports.runAce.subscribe(runace);

app.ports.prompt.subscribe( (prompt) => {
    let value = window.prompt(prompt);
    app.ports.tags.send(value);
});

app.ports.setupAce.subscribe((content) => {
    creationObserver = new MutationObserver(function(mutations) {
	
	let editor = document.getElementById("editor-post-title");
	if (editor) {
	    runace(content); 
	}
	
    });
    creationObserver.observe(document, {attributes: false, childList: true, characterData: false, subtree:true});
});

app.ports.addImgToAce.subscribe(img_id => {
    let editor = ace.edit("editor-post-content");

    if (editor) {
	editor.insert('<img src="/api/pictures/' + img_id +'" />');

    } else alert("Didn't find ace editor");
})
