import * as React from "react";
import { AudioFile } from "./Types";
import { Option, some, none } from "fp-ts/Option";

interface Props {
    // handleSelectedAudioFile: (e: Event) => AudioFile
    x: string
    y: number
}

const getSelectedFile = (e: Event): Option<File> => {
    const element = e.currentTarget as HTMLInputElement;
    const filelist: FileList | null = element.files;
    if (filelist === null) return none;
    const file: File | null = filelist.item(0);
    if (file === null) return none;
    return some(file);

}

export const FileSelector = (props: Props) => {
    return <div>
        <button>select file</button>
        <span>{props.x}</span>
        <span>{props.y}</span>
    </div>

}