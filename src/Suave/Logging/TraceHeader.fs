namespace Suave.Logging

open Suave
open Suave.Utils
open Suave.Utils.RandomExtensions

/// A record that keeps track of what request this is.
/// In an uint64 there are 18 446 744 073 709 551 616 number
/// of possible values, so you can be fairly certain a given request
/// id is unique, given a good random number generator.
type TraceHeader =
  { /// If this is the 'first' traced request, then trace_id equals
    /// req_id. If it's the second, then trace_id = req_parent_id
    /// or otherwise third or later then trace_id, req_id and req_parent_id
    /// are all disjunct
    traceId      : uint64

    /// The request id assigned when suave received the http request
    /// In ZipKin/Dapper-speak, this is the span id
    reqId        : uint64

    /// possibly a parent
    /// In ZipKin/Dapper-speak, this is the span parent id
    reqParentId : uint64 option }

  static member traceId_     = Property (fun x -> x.traceId) (fun v x -> { x with traceId = v })
  static member reqId_       = Property (fun x -> x.reqId) (fun v x -> { x with reqId = v })
  static member reqParentId_ = Property (fun x -> x.reqParentId) (fun v x -> { x with reqParentId = v })

  /// The empty trace header has zeroes for trace and request id.
  static member empty =
    { traceId      = 0UL
      reqId        = 0UL
      reqParentId = None }

  /// Create a new `TraceHeader` with the given `trace_id` and `span_parent_id`.
  /// This generates a new id and places it in `trace_id` AND `req_id` if no
  /// `trace_id` parameter is supplied. Unless `span_parent_id` is given, that
  /// field is defaulted to None, as suave cannot know the "origin span", so to
  /// speak.

  static member mk traceId spanParentId =
    let newId = Globals.random.NextUInt64()
    { traceId     = defaultArg traceId newId
      reqId       = newId
      reqParentId = spanParentId }
