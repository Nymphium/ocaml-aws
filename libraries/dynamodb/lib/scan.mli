type input = Types.ScanInput.t
type output = Aws.Json.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error