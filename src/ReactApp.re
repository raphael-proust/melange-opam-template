module App = {
   let make_grid = (comment, board) =>
      [ <hr/>
      , <div> { React.string(comment) } </div>
      , <table> {
         World.M.Space.offsets
         ->Belt.List.map(fun (horizontal) =>
            <tr> {
               World.M.Space.offsets
               ->Belt.List.map(fun (vertical) =>
                  <td> {
                     let r : World.M.Symbol.t =
                        World.M.Board.read (board) ({World.M.Space.vertical: vertical, horizontal}) ;
                     React.string(World.M.Symbol.to_string(r))
                  }</td>)
               ->Belt.List.toArray
               ->React.array}</tr>)
         ->Belt.List.toArray
         ->React.array}
         </table>
      ]
   ;

	// This sample forces an import of Belt.*, so that CI builds can ensure that
	// Melange has been installed correctly for JS bundlers to be able to find it.
	[@react.component]
		let make = () =>
		World.boards
		->Belt.List.map(fun ((comment : string, board : World.M.Board.t)) => make_grid (comment, board))
		->Belt.List.flatten
		->Belt.List.toArray
		->React.array;
};

ReactDOM.querySelector("#root")
->(
		fun
		| Some(root) => ReactDOM.render(<App />, root)
		| None =>
		Js.Console.error(
			"Failed to start React: couldn't find the #root element",
			)
  );
