INTERFACE zif_excel_ixml_document
  PUBLIC.

  INTERFACES zif_excel_ixml_node.

  ALIASES append_child
    FOR zif_excel_ixml_node~append_child.
  ALIASES get_first_child
    FOR zif_excel_ixml_node~get_first_child.
  ALIASES set_namespace_prefix
    FOR zif_excel_ixml_node~set_namespace_prefix.

  METHODS create_element
    IMPORTING
      !name TYPE string
*      !NAMESPACE type STRING default ''
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_element.

  METHODS create_simple_element
    IMPORTING
      !name TYPE string
*      !NAMESPACE type STRING default ''
      !parent TYPE REF TO zif_excel_ixml_node
*      !VALUE type STRING default ''
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_element.

  METHODS create_simple_element_ns
    IMPORTING
      !name TYPE string
      !parent TYPE REF TO zif_excel_ixml_node
      !prefix TYPE string DEFAULT ''
*      !URI type STRING default ''
*      !VALUE type STRING default ''
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_element.

  METHODS find_from_name
    IMPORTING
*      !DEPTH type I default 0
      !name TYPE string
*      !NAMESPACE type STRING default ''
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_element.

  METHODS find_from_name_ns
    IMPORTING
*      !DEPTH type I default 0
      !name TYPE string
      !uri TYPE string DEFAULT ''
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_element.

  METHODS get_elements_by_tag_name
    IMPORTING
      !depth TYPE i DEFAULT 0
      !name TYPE string
      !namespace TYPE string DEFAULT ''
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_node_collection.

  METHODS get_elements_by_tag_name_ns
    IMPORTING
      !depth TYPE i DEFAULT 0
      !name TYPE string
      !uri TYPE string DEFAULT ''
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_node_collection.

  METHODS get_root_element
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_element.

  METHODS set_encoding
    IMPORTING
      !encoding TYPE REF TO zif_excel_ixml_encoding.

  METHODS set_standalone
    IMPORTING
      !standalone TYPE abap_bool.

ENDINTERFACE.
