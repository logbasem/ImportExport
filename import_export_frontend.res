//This file contains pieces of frontend code that I wrote for the import export backend code, collected from a file in a larger collaborative project

// Import
  let (importingSources, setImportingSources) = React.useState(() => false)

  let mkExportUrl = StdLib.Config.config.baseUrl ++ "/import-source/import/source/export/exportedParams.xlsx"

  let handleImport = (blob: Fetch.blob) => {
    setImportingSources(_ => true)
    SourceFetch.importSourceLister.importFullimportSource(blob)->Js.Promise.then_(result => {
      switch result {
      | Belt.Result.Ok() =>
        fetchimportSources()
        Ui.Toast.success("Successfully imported import sources")
      | Belt.Result.Error(msg) =>
        Ui.Toast.errorWithMoreInfo("Error importing import sources", msg)
      }
      setImportingSources(_ => false)
      Js.Promise.resolve()
    }, _)->Js.Promise.catch(ex => {
      switch Js.Json.stringifyAny(ex) {
      | None => Ui.Toast.error("Unable to import import sources")
      | Some(msg) => Ui.Toast.errorWithMoreInfo("Unable to import import sources", msg)
      }
      setImportingSources(_ => false)
      Js.Promise.resolve()
    }, _)->ignore
  }

<div className="bs3">
      <ParametersImportExport
        exportUrl={mkExportUrl}
        onImport={f => handleImport(f)}
        loading={importingSources}
        key="import-export"
      />
    </div>
