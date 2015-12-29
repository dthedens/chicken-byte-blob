# byte-blob

{{byte-blob}} aims to provide a SRFI-1-inspired API for manipulating
byte vectors encoded as blobs. In addition it borrows inspiration from
the Haskell bytestring library (http://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString.html).

## Library Procedures

### Predicates

<procedure>(byte-blob? X) => BOOL</procedure>

Returns `#t` if the given object is a byte-blob, `#f` otherwise.

<procedure>(byte-blob-empty? BYTE-BLOB) => BOOL</procedure>

Returns `#t` if the given byte-blob is empty, `#f` otherwise.

### Constructors

<procedure>(byte-blob-empty) => BYTE-BLOB</procedure>

Returns an empty byte-blob.

<procedure>(byte-blob-replicate N V) => BYTE-BLOB</procedure>

Returns a byte-blob of length `N`, where each element is `V`.

<procedure>(byte-blob-cons X BYTE-BLOB) => BYTE-BLOB</procedure>

Analogous to list cons, but of complexity O(N), as it requires copying
the elements of the byte-blob argument.

<procedure>(blob->byte-blob BLOB) => BYTE-BLOB</procedure>

Returns a byte-blob containing the elements of the given blob.

<procedure>(list->byte-blob LIST) => BYTE-BLOB</procedure>

Returns a byte-blob containing the elements of `LIST`.

<procedure>(string->byte-blob STRING) => BYTE-BLOB</procedure>

Returns a byte-blob containing the elements of `STRING`.

### Accessors

<procedure>(byte-blob-length BYTE-BLOB) => INTEGER</procedure>

Returns the number of elements contained in the given byte-blob.

<procedure>(byte-blob-car BYTE-BLOB) => X</procedure>

Returns the first element of a byte-blob. The argument byte-blob must
be non-empty, or an exception will be thrown.

<procedure>(byte-blob-cdr BYTE-BLOB) => BYTE-BLOB</procedure>

Returns a byte-blob that contains the elements after the first element
of the given byte-blob. The argument byte-blob must be non-empty, or
an exception will be thrown.

<procedure>(byte-blob-ref BYTE-BLOB I) => BYTE</procedure>

Returns the i-th element of the given byte-blob as a signed byte.

<procedure>(byte-blob-uref BYTE-BLOB I) => BYTE</procedure>

Returns the i-th element of the given byte-blob as an unsigned byte.

### Transformers

<procedure>(byte-blob-set! BYTE-BLOB I V) => VOID</procedure>

Sets the i-th element of the given byte-blob to the signed byte `V`.

<procedure>(byte-blob-uset! BYTE-BLOB I V) => VOID</procedure>

Sets the i-th element of the given byte-blob to the unsigned byte `V`.

<procedure>(byte-blob-append BYTE-BLOB BYTE-BLOB) => BYTE-BLOB </procedure>

Appends two byte-blobs together.

<procedure>(byte-blob-reverse BYTE-BLOB) => BYTE-BLOB</procedure>

Returns a byte-blob that contains the elements of the given byte-blob
in reverse order.

<procedure>(byte-blob-intersperse BYTE-BLOB BYTE) => BYTE-BLOB</procedure>

Returns a byte-blob with the given byte placed between the elements of
the given byte-blob.  

<procedure>(byte-blob-map F BYTE-BLOB) => BYTE-BLOB</procedure>

Returns a byte-blob obtained by applying `F` to each element of the
given byte-blob.

<procedure>(byte-blob->blob BYTE-BLOB) => BLOB</procedure>

Returns the underlying Scheme blob object.

<procedure>(byte-blob->list BYTE-BLOB [F]) => LIST</procedure>

Returns a list containing the elements of the given byte-blob. If
procedure `F` is provided as a second argument, it is applied to
every element of the returned list.

<procedure>(byte-blob->string BYTE-BLOB) => STRING</procedure>

Returns a string containing the elements of the given byte-blob.

### Subsequences

<procedure>(byte-blob-take BYTE-BLOB N) => BYTE-BLOB</procedure>

Returns the prefix of the given byte-blob of length `N`. 

<procedure>(byte-blob-drop BYTE-BLOB N) => BYTE-BLOB</procedure>

Returns the suffix of the given byte-blob after the first `N` elements. 

<procedure>(byte-blob-span BYTE-BLOB START END) => BYTE-BLOB</procedure>

Returns the subsequence of the give byte-blob from position `START`
to position `END`.

### Fold

<procedure>(byte-blob-fold-left F INIT BYTE-BLOB) => VALUE</procedure><br>
<procedure>(byte-blob-fold-right F INIT BYTE-BLOB) => VALUE</procedure><br>

Given a procedure of two arguments, a starting value, and a byte-blob,
reduces the byte-blob using the supplied procedure, from left to
right, or right to left, respectively.

### Find

<procedure>(byte-blob-find NEEDLE HAYSTACK) => LIST</procedure>

Finds all non-overlapping instances of the byte-blob `NEEDLE` in the
byte-blob `HAYSTACK`. The first element of the returned list is the
prefix of `HAYSTACK` prior to any matches of `NEEDLE`.  The second
is a list of lists.

The first element of each pair in the list is a span from the
beginning of a match to the beginning of the next match, while the
second is a span from the beginning of the match to the end of the
input.


### I/O

<procedure>(file->byte-blob FILENAME [MODE]) => BYTE-BLOB</procedure>

Returns a byte-blob with the contents of the given file.

`MODE` is an optional argument that can be one of `#:text` or
`#:binary` to specify text or binary mode on Windows.

<procedure>(byte-blob-read PORT N) => BYTE-BLOB</procedure>

Reads a byte-blob of length `N` from the given port.  Currently, the
port must support the `port->fileno` procedure, which means that
string ports are not supported (in that particular case, procedure
{{string->byte-blob}} can be used for converting strings to byte blobs).

<procedure>(byte-blob-write PORT BYTE-BLOB) => UNDEFINED</procedure>

Writes the given byte-blob to the given port. Currently, the port must
support the `port->fileno` procedure, which means that string ports
are not supported (in that particular case, procedure
{{string->byte-blob}} can be used for converting strings to byte
blobs).

### SRFI-4 transformers

<procedure>(u8vector->byte-blob   U8VECTOR) => BYTE-BLOB</procedure><br>
<procedure>(s8vector->byte-blob   S8VECTOR) => BYTE-BLOB</procedure><br>
<procedure>(u16vector->byte-blob  U16VECTOR) => BYTE-BLOB</procedure><br>
<procedure>(s16vector->byte-blob  S16VECTOR) => BYTE-BLOB</procedure><br>
<procedure>(u32vector->byte-blob  U32VECTOR) => BYTE-BLOB</procedure><br>
<procedure>(s32vector->byte-blob  S32VECTOR) => BYTE-BLOB</procedure><br>
<procedure>(f32vector->byte-blob  F32VECTOR) => BYTE-BLOB</procedure><br>
<procedure>(f64vector->byte-blob  F64VECTOR) => BYTE-BLOB</procedure><br>
<procedure>(byte-blob->u8vector  BYTE-BLOB) => U8VECTOR</procedure><br>
<procedure>(byte-blob->s8vector  BYTE-BLOB) => S8VECTOR</procedure><br>
<procedure>(byte-blob->u16vector BYTE-BLOB) => U16VECTOR</procedure><br>
<procedure>(byte-blob->s16vector BYTE-BLOB) => S16VECTOR</procedure><br>
<procedure>(byte-blob->u32vector BYTE-BLOB) => U32VECTOR</procedure><br>
<procedure>(byte-blob->s32vector BYTE-BLOB) => S32VECTOR</procedure><br>
<procedure>(byte-blob->f32vector BYTE-BLOB) => F32VECTOR</procedure><br>
<procedure>(byte-blob->f64vector BYTE-BLOB) => F64VECTOR</procedure><br>


## License

>
> Based on ideas from the Haskell
> [[http://www.cse.unsw.edu.au/~dons/fps.html|bytestring]] library.  
> 
> The code for `byte-blob-find` is based on code from the Haskell Text
> library by Tom Harper and Bryan O'Sullivan.
> 
>  Copyright 2009-2015 Ivan Raikov
>
>
>  This program is free software: you can redistribute it and/or modify
>  it under the terms of the GNU Lesser General Public License as
>  published by the Free Software Foundation, either version 3 of the
>  License, or (at your option) any later version.
>
>  This program is distributed in the hope that it will be useful, but
>  WITHOUT ANY WARRANTY; without even the implied warranty of
>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
>  General Public License for more details.
>
>  A full copy of the GPL license can be found at
>  <http://www.gnu.org/licenses/>.
>
