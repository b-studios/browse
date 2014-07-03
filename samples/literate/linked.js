function fullUri(ref, query) {
	var loc = /^([^\?#]+)/.exec(location.href)[1];
	return ref ? loc + (query ? "?id=" : "#") + $(ref).attr("id") : loc;
}
function updateExportUri(ref) {
	$("#export span.src").text(fullUri(ref, true))
}
var last_def = false;
function scrollTo(def) {
	$(window).scrollTo( def, 300, { 
		onAfter: function() {
			if (location.search.match(/^\?id=/)) {
				if (last_def)
					last_def.unhighlight();
				def.highlight();
			} else {
				document.location = "#" + def.attr("id");
			}
			last_def = def;
			updateExportUri(def);
		}, axis: "y"
	});
	return false;
}

function addInteractivity(dom) {

	var $dom = $(dom);

	// highlight references to the hovered definition
	$dom.find("code [id]").live("mouseover",
		function() { $(this).references().highlight() }
	).live("mouseout",
		function() { $(this).references().unhighlight() }
	).live("click", function() { $(this).attr("href") ? false : scrollTo($(this)) });
	
	// highlight the definition to the hovered reference
	$dom.find("a[href^='#']").live("mouseover",
		function() { $(this).definition().highlight() }
	).live("mouseout",
		function() { $(this).definition().unhighlight() }
	).live("click",
		function() { return scrollTo($(this).definition()); }
	);
	
	// add quick tips based on the title attribute
	$dom.find('[title]').live("mouseover", function() {
		if ($(this).data('qtip') !== 'object') {
			$(this).qtip({
				style: {
					name: 'dark', 
					tip: true, 
					border: { width: 7, radius: 5 }, 
					width: { max: 500 }
				},
				content: { prerender: true },
				show: { delay: { length: 0 } },
				position: { corner:	{ tooltip: "bottomLeft", target: "topMiddle" } }
			}).qtip("show");
		}
	});
}

/**
 * Uses the very simple heuristic that a comment always
 * preceeds the code it addresses.
 */
function splitIntoCommentsAndCode(codeDom) {
	
	var doc = $(codeDom).get(0);
	
	// aggregate all nodes until a comment is seen
	var last = { comment: "", code: [] };
	var aggr = [];
	
	// only treat doc comments as comments
	function isComment(node) {
		return $(node).hasClass("comment") && (/^\/\*/.exec($(node).text()) !== null)
	}

	$(doc.childNodes).each(function (i, el) {
		if (isComment(el)) {
			aggr.push(last);
			last = { comment: el.innerHTML, code: [] }
		} else {
			last.code.push(el)
		}
	});
	
	aggr.push(last);
	return aggr;
}

// merges comments and removes the first one if it is empty
function cleanupComments(doc) {

	var result = [],
			last  = null,
			i, el;

	for (i = 0; i < doc.length; i++) {
		el = doc[i];

		if (last == null) {
			last = el
			continue;
		}

		if (el.code.length === 0 && el.comment === "")
			continue;

		if (last.code.length == 0 || el.code.length == 0) {
			last.code = last.code.concat(el.code)
			last.markup += el.markup
			last.comment += el.comment
		} else {
			result.push(last);
			last = el;
		}

	}

	result.push(last);
	return result;
}

/**
 * Annotates each block with the markdown generated html corresponding to the comment
 *
 * input:   [{ comment: "/** foo *bar* ...",  code: [] }, ...]
 * output:  [{ comment: "/** foo *bar* ...",  code: [], markup: <html-node> }, ...]
 */
function annotateWithHtml(doc) {
	$(doc).each(function (i, el) {
		 el.markup = Markdown.toHTML(htmlDecode(extractCommentText(el.comment)));
	})
}

/**
 * Parses the comment and removes comment syntax to extract the contained text
 */
function extractCommentText(text) {
	
	var lines = text.split("\n")

	var start = /^\/\*+/,
			lineStart = /^[ \t]*\*[ ]?/,
			end = /\*+\//;

	for (var i = 0; i < lines.length; i++) {
		
		if (i === 0) {
			lines[0] = lines[0].replace(start, "");
		}
		
		if (i === lines.length - 1) {
			lines[i] = lines[i].replace(end, "");
			lines[i] = lines[i].replace(lineStart, "");
		} else {
			lines[i] = lines[i].replace(lineStart, "");
		}
		
	}
	
	return lines.join("\n");
}

function htmlDecode(text) {
		var codes = {
				apos : '\'',
				amp : '&',
				lt : '<',
				gt : '>'
		}

		var r = new RegExp('&(' + Object.getOwnPropertyNames(codes).join('|') +');', 'g')

		return text.replace(r, function(_, code) {
			return codes[code];
		})
}

/**
 * Renders a div-based representation grouping code and comments into sections
 */
function renderDiv(title, doc) {

	var table = $("<article>")
		.attr("id", "documentation");

	var head = $("<section>")
			.append($("<div>")
				.append($("<h1>").html(title))
				.addClass("docs"))
			.append($("<div>")
				.addClass("code"))
	
	table.append(head);

	$(doc).map(function (i, el) {
		var row = $("<section>")
		
		$("<div>")
			.html(el.markup)
			.addClass("docs")
			.appendTo(row);
		
		$("<div>")
			.append($("<code>").append(el.code))
			.addClass("code")
			.appendTo(row);
			
		return row;
		
	}).appendTo(table);
	
	return table;
}

function toLiterateProgramming(docDom) {
	var $doc = $(docDom);

	var blocks = splitIntoCommentsAndCode($doc);
	annotateWithHtml(blocks);
	
	blocks = cleanupComments(blocks);

	var literate = renderDiv(document.title, blocks);

	$(document.body).append(literate);

	addInteractivity(".code");

	$doc.hide();
}

$(window).load(function() {
	var id = id = /^\?id=(.+)$/.exec(location.search);
	
	if(top != window) { // if showing in frame
		if (id) scrollTo($("#" + id[1]));

		// display info bar when mouse in frame
		var pop_out = $('<div class="tool" id="pop-out"><input type="submit" value="Pop Out" />Source published by Scala X-Ray</div>')
		$("body").append(pop_out);
		$(window.document).hover(
			function() { pop_out.stop(false, true); pop_out.fadeIn() },
			function() { pop_out.stop(false, true); pop_out.fadeOut() }
		);
		pop_out.find("input").click(function() {
			window.open(fullUri(last_def));
			return false;
		});
	} else { // if not showing in a frame
		if (id)	// redirect to #id if given id param
			window.location = fullUri($("#" + id[1]));
	}

	// addInteractivity("body > pre");
	toLiterateProgramming("body > pre");
	$(document.body).addClass("literate");
});

jQuery.fn.extend({
	definition: function() {
		var target = document.getElementById(this.attr("href").replace(/^#/, ''));
		return target ? $(target) : $(this);
	},
	references: function() {
		return $("a[href$='#" + this.attr('id') +"']")
	},
	highlight:	function() {
		return this.addClass("highlighted");
	},
	unhighlight: function() {
		return this.removeClass("highlighted");
	}
})
