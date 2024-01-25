CLASS zcl_excel_ixml DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml.

    CLASS-METHODS create
      IMPORTING
        !type TYPE i DEFAULT 0
      RETURNING
        VALUE(rval) TYPE REF TO zif_excel_ixml.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA singleton TYPE REF TO zif_excel_ixml.
ENDCLASS.


CLASS zcl_excel_ixml IMPLEMENTATION.
  METHOD create.
    IF singleton IS NOT BOUND.
      singleton = lcl_ixml=>get_singleton( ).
    ENDIF.
    rval = singleton.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_document.
    rval = singleton->create_document( ).
  ENDMETHOD.

  METHOD zif_excel_ixml~create_encoding.
    rval = singleton->create_encoding( byte_order    = byte_order
                                       character_set = character_set ).
  ENDMETHOD.

  METHOD zif_excel_ixml~create_parser.
    rval = singleton->create_parser( document       = document
                                     istream        = istream
                                     stream_factory = stream_factory ).
  ENDMETHOD.

  METHOD zif_excel_ixml~create_renderer.
    rval = singleton->create_renderer( document = document
                                       ostream  = ostream ).
  ENDMETHOD.

  METHOD zif_excel_ixml~create_stream_factory.
    rval = singleton->create_stream_factory( ).
  ENDMETHOD.
ENDCLASS.
