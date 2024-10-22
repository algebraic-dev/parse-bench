import Lean.Data.Parsec
import Lean.Data.Parsec.ByteArray

open Lean.Parsec
open Lean.Parsec.ByteArray

-- The simple Http Request structure.
structure HttpRequest where
  method : String
  uri : String
  version : (Nat × Nat)
  headers : Array (String × String)
  data: Array ByteArray

@[inline]
def char (c: Char) : Parser Unit := skipByte c.toUInt8

-- The parser

/--
token      = 1*<any CHAR except CTLs or separators>
CTL        = <any US-ASCII control character (octets 0 - 31) and DEL (127)>
separators = "(" | ")" | "<" | ">" | "@"
           | "," | ";" | ":" | "" | <">
           | "/" | "[" | "]" | "?" | "="
           | "{" | "}" | SP | HT
--/
def isTokenCharacter (c : UInt8) : Bool :=
  --let separators := #['(', ')', '<', '>', '@', ',', ';', ':', '"', '/', '[', ']', '?', '=', '{', '}', ' ']
  c > 31 && c != '('.toUInt8 && c != ')'.toUInt8 && c != '<'.toUInt8 && c != '>'.toUInt8 &&
    c != '@'.toUInt8 && c != ','.toUInt8 && c != ';'.toUInt8 && c != ':'.toUInt8 &&
    c != '"'.toUInt8 && c != '/'.toUInt8 && c != '['.toUInt8 && c != ']'.toUInt8 &&
    c != '?'.toUInt8 && c != '='.toUInt8 && c != '{'.toUInt8 && c != '}'.toUInt8 && c != ' '.toUInt8

@[inline]
def token : Parser String := many1Chars (Char.ofUInt8 <$> satisfy isTokenCharacter)

def methodParser : Parser String
  := pstring "HEAD"
  <|> pstring "GET"
  <|> pstring "POST"
  <|> pstring "PUT"
  <|> pstring "DELETE"
  <|> pstring "OPTIONS"
  <|> pstring "CONNECT"
  <|> pstring "TRACE"
  <|> pstring "PATCH"

@[inline]
def spaces : Parser Unit :=
  many (satisfy (λ c => c = ' '.toUInt8)) *> pure ()

@[inline]
def uriParser : Parser String :=
  many1Chars (Char.ofUInt8 <$> satisfy (λ c => c ≠ ' '.toUInt8 && c ≠ '\n'.toUInt8 && c ≠ '\r'.toUInt8))

@[inline]
def versionParser : Parser (Nat × Nat) := do
  skipString "HTTP/"
  let major ← (Char.toNat · - Char.toNat '0') <$> digit
  char '.'
  let minor ← (Char.toNat · - Char.toNat '0') <$> digit
  return (major, minor)

def headerParser : Parser (String × String) := do
  let name ← token
  _ ← char ':'
  spaces
  let value ← many1Chars (Char.ofUInt8 <$> satisfy (λ c => c ≠ '\n'.toUInt8 ∧ c ≠ '\r'.toUInt8))
  _ ← char '\r'
  _ ← char '\n'
  return (name, value)

@[inline]
def headersParser : Parser (Array (String × String)) :=
  many (headerParser <* spaces)

def requestLineParser : Parser (String × String × (Nat × Nat)) := do
  let method ← methodParser
  spaces
  let uri ← uriParser
  spaces
  let version ← versionParser
  skipByte '\r'.toUInt8
  skipByte '\n'.toUInt8
  return (method, uri, version)

def chunk : Parser ByteArray := do
    let size ← decodeHex <$> many1Chars hexDigit
    skipString "\r\n"
    let data ← take size
    skipString "\r\n"
    return data
  where
    decodeHex (s: String) := s.foldl (λacc c => acc * 16 + decodeHexDigit c) 0
    decodeHexDigit (c : Char) : Nat :=
      if c  ≤ '9' then (c.val - '0'.val).toNat
      else if c  ≤ 'F' then 10 + (c.val - 'A'.val).toNat
      else 10 + (c.val - 'a'.val).toNat

@[inline]
def bodyParser : Parser String :=
  manyChars (Char.ofUInt8 <$> satisfy (λ _ => true))

def httpRequestParser : Parser HttpRequest := do
  let (method, uri, version) ← requestLineParser
  let headers ← headersParser
  skipString "\r\n"
  let data ← many chunk
  eof
  return { method, uri, version, headers, data }

def parse (input: ByteArray) : IO Unit := do
  match httpRequestParser input.mkIterator with
  | .success _ result => IO.println s!"Parsed {result.data.size} chunks and {result.headers.size} headers"
  | .error l err => IO.println s!"Error parsing HTTP Request at {l.idx}: {err}"

def main : IO Unit := do
  let input ← IO.FS.readBinFile "./test.txt"
  let t1 ← IO.monoMsNow
  parse input
  let t2 ← IO.monoMsNow
  IO.println s!"Parsing took {t2 - t1}ms"
