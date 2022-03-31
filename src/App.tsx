import * as React from "react";
import * as ReactDOM from "react-dom/client";
import { Option, none } from "fp-ts/Option";
import { AudioFile } from "./Types";
import { FileSelector } from "./FileSelector";

interface State {
    audioFile: Option<AudioFile>
}

interface Props {}

const Root = (_props: Props) => {
    const [state, setState] = React.useState<State>({ audioFile: none });
    return <FileSelector x="a" y={81} />;
}

ReactDOM.createRoot(
    document.getElementById("app") as HTMLElement,
).render(
    <Root />
);