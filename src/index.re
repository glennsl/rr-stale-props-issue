
external document : Js.t {..} = "" [@@bs.val];
external window : Js.t {..} = "" [@@bs.val];

module Button = {
  type state = {
    element: ref (option Dom.element),
    clickHandler: ref (option (unit => unit))
  };

  let component = ReasonReact.reducerComponentWithRetainedProps "Button";
  let make ::count ::onChange _children => {

    let handleClick () =>
      onChange (count + 1);

    let updateEventListener state =>
      switch !state.element {
      | Some element =>
        switch !state.clickHandler {
        | Some oldHandler => 
          (ReactDOMRe.domElementToObj element)##removeEventListener "click" oldHandler
        | None => ()
        };
        state.clickHandler := Some handleClick;
        (ReactDOMRe.domElementToObj element)##addEventListener "click" handleClick
      | None => ()
      };

    {
      ...component,

      retainedProps: onChange,
      initialState: fun () => { element: ref None, clickHandler: ref None },
      reducer: fun () _state => ReasonReact.NoUpdate,

      didMount: fun self => {
        updateEventListener self.state;
        ReasonReact.NoUpdate
      },

      didUpdate: fun {oldSelf, newSelf} =>
        if (oldSelf.retainedProps !== newSelf.retainedProps ||
            oldSelf.state.element !== newSelf.state.element) {
          updateEventListener newSelf.state
        },

      render: fun self =>
        <button ref=(self.handle (fun el {state} => state.element := Js.Null.to_opt el)) >
          (ReasonReact.stringToElement {j|Click to increment count: $count|j})
        </button>
    }
  };
};

module App = {
  let component = ReasonReact.reducerComponent "Button";
  let make _children => {
    ...component,

    initialState: fun () => 0,

    reducer: fun newCount _ => ReasonReact.Update newCount,

    render: fun self =>
      <div>
        <div>
          (ReasonReact.stringToElement (string_of_int self.state))
        </div>
        <Button count=self.state onChange=(self.reduce (fun newCount => newCount)) />
      </div>
  };
};

ReactDOMRe.renderToElementWithId <App /> "index";
