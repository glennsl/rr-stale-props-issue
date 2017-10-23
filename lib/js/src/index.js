// Generated by BUCKLESCRIPT VERSION 1.9.3, PLEASE EDIT WITH CARE
'use strict';

var Block       = require("bs-platform/lib/js/block.js");
var Curry       = require("bs-platform/lib/js/curry.js");
var React       = require("react");
var Pervasives  = require("bs-platform/lib/js/pervasives.js");
var ReactDOMRe  = require("reason-react/lib/js/src/reactDOMRe.js");
var ReasonReact = require("reason-react/lib/js/src/reasonReact.js");

var component = ReasonReact.reducerComponent("Button");

function make(count, onChange, _) {
  var handleClick = function () {
    console.log(count);
    return Curry._1(onChange, count + 1 | 0);
  };
  var newrecord = component.slice();
  newrecord[/* didMount */4] = (function (self) {
      var match = self[/* state */4][/* element */0][0];
      if (match) {
        match[0].addEventListener("click", handleClick);
      }
      return /* NoUpdate */0;
    });
  newrecord[/* render */9] = (function (self) {
      return React.createElement("div", {
                  ref: Curry._1(self[/* handle */0], (function (el, param) {
                          param[/* state */4][/* element */0][0] = el === null ? /* None */0 : [el];
                          return /* () */0;
                        }))
                }, "Click to increment count");
    });
  newrecord[/* initialState */10] = (function () {
      return /* record */[/* element */[/* None */0]];
    });
  newrecord[/* reducer */12] = (function (_, _$1) {
      return /* NoUpdate */0;
    });
  return newrecord;
}

var Button = /* module */[
  /* component */component,
  /* make */make
];

var component$1 = ReasonReact.reducerComponent("Button");

function make$1() {
  var newrecord = component$1.slice();
  newrecord[/* render */9] = (function (self) {
      return React.createElement("div", undefined, React.createElement("div", undefined, Pervasives.string_of_int(self[/* state */4])), ReasonReact.element(/* None */0, /* None */0, make(self[/* state */4], Curry._1(self[/* reduce */3], (function (newCount) {
                                return newCount;
                              })), /* array */[])));
    });
  newrecord[/* initialState */10] = (function () {
      return 0;
    });
  newrecord[/* reducer */12] = (function (_, newCount) {
      return /* Update */Block.__(0, [newCount]);
    });
  return newrecord;
}

var App = /* module */[
  /* component */component$1,
  /* make */make$1
];

ReactDOMRe.renderToElementWithId(ReasonReact.element(/* None */0, /* None */0, make$1(/* array */[])), "index");

exports.Button = Button;
exports.App    = App;
/* component Not a pure module */