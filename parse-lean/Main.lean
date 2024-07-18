import Parse
import Parse.DSL

namespace Description

open Parse.DSL

parser HttpGrammar in Lean where
  def type : u8
  def method : u8
  def major : u8
  def minor : u8
  def url : span
  def chunkData : span
  def prop : span
  def value : span

  def contentLength : u64
  def chunkLength : u64

  set digit := ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]
  set hexDigit := ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" "a" "b" "c" "d" "e" "f"]
  set ws := [" " "\x09"]

  set token := [
    "!" "#" "$" "%" "&" "\"" "*" "+" "-" "." "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D"
    "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "\\" "^"
    "_" "`" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
    "w" "x" "y" "z" "|" "~"
  ]

  callback endField
  callback endRequest
  callback endRequestLine [ method major minor ]

  node method where
    switch (store method beforeUrl)
      | "HEAD" => 0
      | "GET" => 1
      | "POST" => 2
      | "PUT" => 3
      | "DELETE" => 4
      | "OPTIONS" => 5
      | "CONNECT" => 6
      | "TRACE" => 7
      | "PATCH" => 8
    otherwise (error 23)

    node beforeUrl where
      is " " (start url url)

    node url where
      peek ' ' (end url endUrl)
      any url

    node endUrl where
      is " " httpVersionStart

    node httpVersionStart where
      is "HTTP/" httpVersionMajor

    node httpVersionMajor where
      is "1" (call (loadNum major) httpVersionDot)
      is "2" (call (loadNum major) httpVersionDot)

    node httpVersionDot where
      is "." httpVersionMinor

    node httpVersionMinor where
      is digit (call (loadNum minor) lineAlmostDone)

    node lineAlmostDone where
      is "\r\n" fieldLineStart

    node fieldLineStart where
      peek '\r' startChunker
      otherwise (start prop fieldLineProp)

    node fieldLineProp where
      peek ':' (end prop fieldLineColon)
      any fieldLineProp
      otherwise (error 1)

    node fieldLineColon where
      is ":" fieldLineOWS
      otherwise (error 2)

    node fieldLineOWS where
      is " " fieldLineOWS
      otherwise (start value fieldLineValue)
      otherwise (error 3)

    node contentLength where
      peek '\r' (end value selectLineEnd)
      is digit (call (mulAdd decimal contentLength) contentLength)
      otherwise (error 5)

    node fieldLineValue where
      peek '\r' (end value selectLineEnd)
      any fieldLineValue

    node selectLineEnd where
      select endField
        | 0 => fieldLineEnd
        default => error 22

    node fieldLineEnd where
      is "\r\n" fieldLineStart

    node startChunker where
      is "\r\n" (call (store chunkLength 0) chunkSize)
      otherwise (error 120)

    node chunk where
      otherwise (call (store chunkLength 0) chunkSize)

    node chunkSize where
      is hexDigit (call (mulAdd hex chunkLength) chunkParseLength)
      otherwise (error 121)

    node chunkParseLength where
      is hexDigit (call (mulAdd hex chunkLength) chunkParseLength)
      otherwise chunkData

    node chunkData where
      is "\r\n" (start chunkData chunkDataInfo)
      otherwise (error 123)

    node chunkDataInfo where
      consume chunkLength (end chunkData chunkDataEnd)
      otherwise (error 124)

    node chunkDataEnd where
      select (read chunkLength)
        | 0 => theEnd
        default => chunkDataSelect

    node chunkDataSelect where
      is "\r\n" chunk
      otherwise (error 125)

    node theEnd where
      is "\r\n" (call endRequest method)

end Description
open Description

structure HttpRequest where
  method : String := ""
  uri : String := ""
  version : (Nat × Nat) := (0, 0)
  headers : Array (String × String) := #[]
  data: Array ByteArray := #[]

structure State where
  req: HttpRequest := {}
  prop: String := ""
  value: String := ""

def String.fromAscii (arr: ByteArray) : String := Id.run $ do
  let mut s := ""
  for byte in arr do s := s.push $ Char.ofNat byte.toNat
  return s

@[inline]
private def onBody (data: ByteArray) (acc: State) : IO (State × Nat) := do
  pure ({acc with req := {acc.req with data := acc.req.data.push data}}, 0)

@[inline]
private def endField (state: State) : IO (State × Nat) := do
  let prop := state.prop
  let value := state.value

  pure ({ state with req := {state.req with headers := state.req.headers.push (prop, value)}, prop := "", value := ""}, 0)

def Method.ofNat : Nat → String
  | 0 => "HEAD"
  | 1 => "GET"
  | 2 => "POST"
  | 3 => "PUT"
  | 4 => "DELETE"
  | 5 => "OPTIONS"
  | 6 => "CONNECT"
  | 7 => "TRACE"
  | 8 => "PATCH"
  | _ => "??"

@[inline]
private def onRequestLine (method: Nat) (major: Nat) (minor: Nat) (acc: State) : IO (State × Nat) := do
  let method := Option.get! $ Method.ofNat method
  return ({acc with req := {acc.req with version := (major, minor), method}}, 0)

@[inline]
private def onEnd (state: State) : IO (State × Nat) := do
  IO.println s!"Parsed {state.req.data.size} chunks and {state.req.headers.size} headers"
  pure (state, 0)

def create : HttpGrammar.Data State :=
  HttpGrammar.create
    (onEndField := endField)
    (onEndRequest := onEnd)
    (onEndRequestLine := onRequestLine)
    (onUrl := toString (λval acc => pure ({acc with req := {acc.req with uri := acc.req.uri.append val}}, 0)))
    (onChunkData := toByteArray (onBody))
    (onProp := toString (λval acc => pure ({acc with prop := acc.prop.append val}, 0)))
    (onValue := toString (λval acc => pure ({acc with value := acc.value.append val}, 0)))
    {}
  where
    @[inline]
    toByteArray func st en bt data := func (bt.extract st en) data

    @[inline]
    toString func st en bt data := func (String.fromAscii $ bt.extract st en) data

    @[inline]
    appendOr (data: Option String) (str: String) : Option String :=
      match data with
      | some res => some $ res.append str
      | none => some str

def parse (ba: ByteArray) : IO Unit := do
  let data := create
  let res ← HttpGrammar.parse data ba

  if res.error ≠ 0 then
    IO.println s!"error {res.error} {res.state}"

def main : IO Unit := do
  let input ← IO.FS.readBinFile "./test.txt"
  parse input
