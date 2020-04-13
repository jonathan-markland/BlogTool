module FileSystemEffects

type FileSaveJob =
    {
        SaveFileContent : string
        SaveAsFilePath  : string
    }

type FileCopyJob =
    {
        CopySourceFilePath : string
        CopyTargetFilePath : string
    }

let SaveFilesNow saveJobs =
    saveJobs |> List.iter (fun saveJob -> System.IO.File.WriteAllText(saveJob.SaveAsFilePath, saveJob.SaveFileContent))

let CopyFilesNow copyJobs =
    copyJobs |> List.iter (fun copyJob -> System.IO.File.Copy(copyJob.CopySourceFilePath, copyJob.CopyTargetFilePath))
