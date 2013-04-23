/* ***** BEGIN LICENSE BLOCK *****
 * Distributed under the BSD license:
 *
 * Copyright (c) 2012, Ajax.org B.V.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Ajax.org B.V. nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL AJAX.ORG B.V. BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *
 * Contributor(s):
 *
 *
 *
 * ***** END LICENSE BLOCK ***** */

define('ace/mode/janus', ['require', 'exports', 'module' , 'ace/lib/oop', 'ace/mode/text', 'ace/tokenizer', 'ace/mode/janus_highlight_rules'], function(require, exports, module) {


var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var Tokenizer = require("../tokenizer").Tokenizer;
var JanusHighlightRules = require("./janus_highlight_rules").JanusHighlightRules;
var Range = require("../range").Range;

var Mode = function() {
    var highlighter = new JanusHighlightRules();

    this.$tokenizer = new Tokenizer(highlighter.getRules());
};
oop.inherits(Mode, TextMode);

(function() {

    this.lineCommentStart = ["//"];
    this.blockComment = {start: "/*", end: "*/"};

    var indentKeywords = {
        "procedure": 1,
        "then": 1,
        "else": 1,
        "fi": -1,
        "do": 1,
        "loop": 1,
        "until": -1
    };
    var outdentKeywords = [
        "else",
        "fi",
        "loop",
        "until"
    ];


    function getNetIndentLevel(tokens) {
        var level = 0;
        for (var i = 0; i < tokens.length; i++) {
            var token = tokens[i];
            if (token.type == "keyword") {
                if (token.value in indentKeywords) {
                    level += indentKeywords[token.value];
                }
            } else if (token.type == "paren.lparen") {
                level ++;
            } else if (token.type == "paren.rparen") {
                level --;
            }
        }
        if (level < 0) {
            return -1;
        } else if (level > 0) {
            return 1;
        } else {
            return 0;
        }
    }

    this.getNextLineIndent = function(state, line, tab) {
        var indent = this.$getIndent(line);
        var level = 0;

        var tokenizedLine = this.$tokenizer.getLineTokens(line, state);
        var tokens = tokenizedLine.tokens;

        if (state == "start") {
            level = getNetIndentLevel(tokens);
        }
        if (level > 0) {
            return indent + tab;
        } else if (level < 0 && indent.substr(indent.length - tab.length) == tab) {
            if (!this.checkOutdent(state, line, "\n")) {
                return indent.substr(0, indent.length - tab.length);
            }
        }
        return indent;
    };

    this.checkOutdent = function(state, line, input) {
        if (input != "\n" && input != "\r" && input != "\r\n")
            return false;

        if (line.match(/^\s*[\)\}\]]$/))
            return true;

        var tokens = this.$tokenizer.getLineTokens(line.trim(), state).tokens;

        if (!tokens || !tokens.length)
            return false;

        return (tokens[0].type == "keyword" && outdentKeywords.indexOf(tokens[0].value) != -1);
    };

    this.autoOutdent = function(state, session, row) {
        var prevLine = session.getLine(row - 1);
        var prevIndent = this.$getIndent(prevLine).length;
        var prevTokens = this.$tokenizer.getLineTokens(prevLine, "start").tokens;
        var tabLength = session.getTabString().length;
        var expectedIndent = prevIndent + tabLength * getNetIndentLevel(prevTokens);
        var curIndent = this.$getIndent(session.getLine(row)).length;
        if (curIndent < expectedIndent) {
            return;
        }
        session.outdentRows(new Range(row, 0, row + 2, 0));
    };

}).call(Mode.prototype);

exports.Mode = Mode;
});


define('ace/mode/janus_highlight_rules', ['require', 'exports', 'module' , 'ace/lib/oop', 'ace/mode/text_highlight_rules'], function(require, exports, module) {

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var JanusHighlightRules = function() {
    var keywords = (
        "procedure|if|then|else|fi|from|do|loop|until|" +
        "local|delocal|call|uncall|skip"
    );
    var builtinConstants = ("nil|true|false");
    var types = ("int|stack");
    var builtinFunctions = (
        "empty|top|size|push|pop|error|" +
        "show|print|printf"
    );

    var keywordMapper = this.createKeywordMapper({
        "variable.language": "this",
        "keyword": keywords,
        "constant.language": builtinConstants,
        "storage.type": types,
        "support.function": builtinFunctions
    }, "identifier");

    this.$rules = {
        "start" : [
            {
                token : "comment", // single line comment
                regex : "\\/\\/.*$"
            }, {
                token : "comment", // multi line comment
                regex : "\\/\\*",
                next : "comment"
            }, {
                token : "string", // single line
                regex : '["](?:(?:\\\\.)|(?:[^"\\\\]))*?["]'
            }, {
                token : "constant.numeric", // integer
                regex : "[+-]?\\d+"
            }, {
                token : keywordMapper,
                regex : "[a-zA-Z_$][a-zA-Z0-9_$]*\\b"
            }, {
                token : "keyword.operator",
                regex : "!|%|&|\\||\\*|/|\\-|\\+|=|!=|<=|>=|<=>|<|>|&&|\\|\\||\\+=|\\-=|\\^="
            }, {
                token : "paren.lparen",
                regex : "[[({]"
            }, {
                token : "paren.rparen",
                regex : "[\\])}]"
            }, {
                token : "text",
                regex : "\\s+"
            }
        ],
        "comment" : [
            {
                token : "comment", // closing comment
                regex : ".*?\\*\\/",
                next : "start"
            }, {
                token : "comment", // comment spanning whole line
                regex : ".+"
            }
        ]
    };
}
oop.inherits(JanusHighlightRules, TextHighlightRules);

exports.JanusHighlightRules = JanusHighlightRules;

});
