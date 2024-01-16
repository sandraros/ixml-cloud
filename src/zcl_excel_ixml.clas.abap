CLASS zcl_excel_ixml DEFINITION
  PUBLIC
  FINAL
  create private .

  PUBLIC SECTION.

  interfaces zIF_excel_IXML .

  class-methods CREATE
    importing
      !TYPE type I default 0
    returning
      value(RVAL) type ref to zIF_excel_IXML .

      PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_excel_ixml IMPLEMENTATION.
  METHOD create.
    rval = new zcl_excel_ixml( ).
  ENDMETHOD.

ENDCLASS.
