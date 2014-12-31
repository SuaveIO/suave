namespace Suave.Logging

open Suave
open Suave.Utils
open Suave.Utils.RandomExtensions

/// A record that keeps track of what request this is.
/// In an uint64 there are 18 446 744 073 709 551 616 number
/// of possible values, so you can be fairly certain a given request
/// id is unique, given a good random number generator.
type TraceHeader =
  { /// if this is the 'first' traced request, then trace_id equals
    /// req_id. If it's the second, then trace_id = req_parent_id
    /// or otherwise third or later then trace_id, req_id and req_parent_id
    /// are all disjunct
    trace_id      : uint64
    /// the request id assigned when suave received the http request
    /// In ZipKin/Dapper-speak, this is the span id
    req_id        : uint64
    /// possibly a parent
    /// In ZipKin/Dapper-speak, this is the span parent id
    req_parent_id : uint64 option }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TraceHeader =
  /// The empty trace header has zeroes for trace and request id.
  let empty =
    { trace_id      = 0UL
      req_id        = 0UL
      req_parent_id = None }

  /// Create a new `TraceHeader` with the given `trace_id` and `span_parent_id`.
  /// This generates a new id and places it in `trace_id` AND `req_id` if no
  /// `trace_id` parameter is supplied. Unless `span_parent_id` is given, that
  /// field is defaulted to None, as suave cannot know the "origin span", so to
  /// speak.
  let mk trace_id span_parent_id =
    let new_id = Globals.random.NextUInt64()
    { trace_id      = trace_id |> Option.or_default new_id
      req_id        = new_id
      req_parent_id = span_parent_id }
