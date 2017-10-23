
external document : Js.t {..} = "" [@@bs.val];
external window : Js.t {..} = "" [@@bs.val];

module Button = {
  type state = {
    element: ref (option Dom.element)
  };

  let component = ReasonReact.reducerComponent "Button";
  let make ::count ::onChange _children => {

    let handleClick _ => {
      Js.log count;
      onChange (count + 1)
    };

    {
      ...component,

      initialState: fun () => { element: ref None },

      reducer: fun () _state => ReasonReact.NoUpdate,

      didMount: fun self => {
        switch !self.state.element {
        | Some element =>
          (ReactDOMRe.domElementToObj element)##addEventListener "click" handleClick;
        | None => ()
        };
        ReasonReact.NoUpdate
      },

      render: fun self =>
        <div ref=(self.handle (fun el {state} => state.element := Js.Null.to_opt el)) >
          (ReasonReact.stringToElement "Click to increment count")
        </div>
    }
  };
};

module App = {
  let component = ReasonReact.reducerComponent "Button";
  let make _children => {
    ...component,

    initialState: fun () => 0,

    reducer: fun _ newCount => ReasonReact.Update newCount,

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
