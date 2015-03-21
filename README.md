Fling, an S-Expression grepping tool
====================================

Berke Durak <berke.durak@gmail.com>

Purpose
-------

This tool is used to select parts of S-Expressions, especially those generated
automatically by Sexplib.

Example
-------

Let's say you have the following type:

    type entry =
      | Account of account
      | Closed of string
    and account =
      {
        holder  : string;
        balance : float;
        opened  : string; (* A date *)
      }

Then Sexplib would serialize a sequence of entries as:

    (Account ((holder Dupont) (balance 1234.5)))
    (Account ((holder Durant) (balance 2345.6)))
    (Closed  Dumont)
    (Account ((holder Dupond) (balance 3456.7)))

If you want to select all the holder names and dates from such a sequence you
can use

    fling "(Tag Account (Seq ((Fields (holder opened) Emit) Newline)))" \
      examples/entries.sexp

Expression
----------

An expression is either:

* A sequence of expressions (Seq (expr1 expr2 ... ))
* An Emit statement
* A Newline statement
* A constant string (Const "foo")
* A tag selector (Tag tag expr)
* A field selector (Fields (field1 field2 ...) expr)

Compilation
-----------

oasis setup && ./configure && make && make install
