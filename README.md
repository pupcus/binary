# binary

clojure utils/tools for reading/writing binary data

This library originated from a blog post I read online:

http://gnuvince.wordpress.com/2009/01/29/reading-binary-data-in-clojure/

Thanks to the author of the above post for sharing his ideas.

## building

   lein deps; lein install

## usage


Generating Byte Arrays

The first thing I needed to do was to pack byte buffers so I could then 

1.  use them to test against the results of encoding data below
2.  use them as original data to test against the results of decoding data below

There is a fairly general to-byte-array method that does this.  It takes a vector of various pieces of data and packs them into a byte [].  

examples:

```
    (to-byte-array [10])
    #<byte[] [B@6d386751>
```

Not very exciting and doesn't show us much. First, to get a look at the byte [] in a more human readable form try this:

```
   (vec (to-byte-array [10]))
   [0 0 0 10]
```

Now, examples of the sorts of things you can put into the vector for packing into the byte[]:

```
   [10 [20 30 40] {:data 0x01020304 :endian :little} (byte 0x11) (short 23) (long 1000) 10.4 (float 10.4) "test" ["test" nil]]
```

A hash map placed in the vector should contain one entry of data (key doesn't matter) and one optional entry for 'endian-ness' :big or :little (default is :big).

The vector can contain: int, long, short, byte, BigInteger, BigDecimal, ratios, char, string, nil, map (as mentioned above), and other nested vectors.

to see it in action:

```
(vec (to-byte-array
            [10 [20 30 40] {:data 0x01020304 :endian :little} (byte 0x11) (short 23) (long 1000) 10.4 (float 10.4) "test" ["test" nil]]))
```

returns:

```
   [0 0 0 10 0 0 0 20 0 0 0 30 0 0 0 40 4 3 2 1 17 0 23 0 0 0 0 0 0 3 -24 64 36 -52 -52 -52 -52 -52 -51 65 38 102 102 116 101 115 116 116 101 115 116 0]
```

so now I can build byte arrays with various 'pieces' of data fairly simply and use that in testing the next set of functionality: encoding/decoding binary data based on a description of how the data is arranged in the buffer.


Encoding/Decoding Binary Data:

To describe how specific binary data should be decoded into a hash map OR to encode a map into a specific binary data structure, you describe the data structure with an array of maps, with each map describing a piece of the binary data, as so:

```
   (def structure  [{:name :command-length    :type :int}
                    {:name :command-id        :type :int :out #(kw-to-id (:command-id %)) :in #(id-to-kw (:command-id %))}
                    {:name :sequence-number   :type :int}
                    {:name :fraction          :type :double}
                    {:name :percentage        :type :double}
                    {:name :average           :type :float}
                    {:name :num-points        :type :int :out #(count (:points %))}
                    {:name :points            :type :int  :number :command-status}
                    {:name :text              :type :vstring :size 44}
                    {:name :body              :type :cstring :size 12}])
```

Each map in this array describes a piece of data to be read/written.  Valid options here are:


     :name    (required)
     :type    (required) 
              :int 
              :long 
              :double 
              :float 
              :cstring (null terminated string) 
              :vstring (consist of an int size followed by size bytes for the text)
              (more to come later but this is all I happen to need atm)

     :in      (optional) (function to apply to data after reading)
     :out     (optional) (function to apply to data before writing)


the 'in'   function is given a map of the data pieces already read up to that point including the data just read (keyed by the name)

the 'out'  function is given a map of the data pieces to be written merged with data pieces that have already been written (in case they were changed or set by functions/kws)

     :number  (optional) (number of things of this type to read, default 1
              *(if (> number 1) <returns a vec of the items> <returns item>) )

     :size    (optional) (only used for strings -- determines the max length)
     
     for :number and :size you can specify a keyword name of an element being read to use for that value
                           (must have already been read in tho)
                           OR you can specify a map ie. {:in function/kw/value :out function/kw/value }
                           (the functions take a data map of values as above)

the functions available are

```
    (decode buf definition)
```

where buf is either java.nio.ByteBuffer, or a plain byte []
      definition is the 'layout' definition of the binary data

decode will return a map of the values decoded keyed by the :name value of the layout definition


```
    (encode definition data)
```

where definition is the 'layout' definition of the binary data
      data is the map of data to pack/encode
      (keyword in data map must match one of the :name values in layout definition)

encode will return a byte [] of the encoded data

see the tests for some trivial examples

## other stuff

explore the src to see what other support functions are available (none are hidden .. use at your own risk :-) )

constructive suggestions are welcome. If you use this and find any bugs please let me know.
  
## License

Copyright (C) 2011 pupcus

Distributed under the Eclipse Public License, the same as Clojure.

USE AS IS. NO GUARANTEES/WARRANTY/ETC

