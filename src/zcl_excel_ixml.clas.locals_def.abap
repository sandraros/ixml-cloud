*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_ixml_attribute DEFINITION DEFERRED.
CLASS lcl_ixml_character_data DEFINITION DEFERRED.
CLASS lcl_ixml_document DEFINITION DEFERRED.
CLASS lcl_ixml_element DEFINITION DEFERRED.
CLASS lcl_ixml_encoding DEFINITION DEFERRED.
CLASS lcl_ixml_factory DEFINITION DEFERRED.
CLASS lcl_ixml_istream_string DEFINITION DEFERRED.
CLASS lcl_ixml_istream_xstring DEFINITION DEFERRED.
CLASS lcl_ixml_named_node_map DEFINITION DEFERRED.
CLASS lcl_ixml_node DEFINITION DEFERRED.
CLASS lcl_ixml_node_collection DEFINITION DEFERRED.
CLASS lcl_ixml_node_iterator DEFINITION DEFERRED.
CLASS lcl_ixml_node_list DEFINITION DEFERRED.
CLASS lcl_ixml_ostream_string DEFINITION DEFERRED.
CLASS lcl_ixml_ostream_xstring DEFINITION DEFERRED.
CLASS lcl_ixml_parser DEFINITION DEFERRED.
CLASS lcl_ixml_renderer DEFINITION DEFERRED.
CLASS lcl_ixml_stream DEFINITION DEFERRED.
CLASS lcl_ixml_stream_factory DEFINITION DEFERRED.
CLASS lcl_ixml_text DEFINITION DEFERRED.
CLASS lcl_ixml_unknown DEFINITION DEFERRED.

INTERFACE lif_ixml_all_friends.
ENDINTERFACE.


CLASS lcl_ixml_root_all DEFINITION.
*    FRIENDS lif_ixml_all_friends.
  PUBLIC SECTION.
    INTERFACES lif_ixml_all_friends.
ENDCLASS.


CLASS lcl_ixml_factory DEFINITION
    INHERITING FROM lcl_ixml_root_all
    CREATE PROTECTED
    FRIENDS zcl_excel_ixml.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml.
*    INTERFACES lif_ixml_all_friends.

    TYPES tv_element_name_id TYPE i.
    TYPES tv_node_id         TYPE i.
    TYPES tv_node_type       TYPE i.
    TYPES tv_namespace_id    TYPE i.
    TYPES tt_node_id         TYPE STANDARD TABLE OF tv_node_id WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_node,
        id        TYPE tv_node_id,
        type      TYPE tv_node_type,
        parent_id TYPE tv_node_id,
        text      TYPE string,
        object    TYPE REF TO object,
      END OF ts_node.
    TYPES tt_node TYPE SORTED TABLE OF ts_node WITH UNIQUE KEY id
                    WITH NON-UNIQUE SORTED KEY by_parent COMPONENTS parent_id.
    TYPES:
      BEGIN OF ts_namespace,
        id  TYPE tv_namespace_id,
        uri TYPE string,
      END OF ts_namespace.
    TYPES tt_namespace TYPE HASHED TABLE OF ts_namespace WITH UNIQUE KEY id
                    WITH UNIQUE HASHED KEY by_uri COMPONENTS uri.
    TYPES:
      BEGIN OF ts_element_name,
        id           TYPE tv_element_name_id,
        name         TYPE string,
        namespace_id TYPE tv_namespace_id,
      END OF ts_element_name.
    TYPES tt_element_name TYPE HASHED TABLE OF ts_element_name WITH UNIQUE KEY id
                    WITH NON-UNIQUE SORTED KEY by_name COMPONENTS name namespace_id.
    TYPES:
      BEGIN OF ts_attribute,
        name  TYPE string,
        value TYPE string,
      END OF ts_attribute.
    TYPES tt_attribute TYPE STANDARD TABLE OF ts_attribute WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_element,
        id         TYPE tv_node_id,
        name_id    TYPE tv_element_name_id,
        attributes TYPE tt_attribute,
        object     TYPE REF TO lcl_ixml_element,
      END OF ts_element.
    TYPES tt_element TYPE HASHED TABLE OF ts_element WITH UNIQUE KEY id
                    WITH NON-UNIQUE SORTED KEY by_name COMPONENTS name_id.
    TYPES:
      BEGIN OF ts_parse_element_level,
        level   TYPE i,
        node_id TYPE tv_node_id,
      END OF ts_parse_element_level.
    TYPES tt_parse_element_level TYPE HASHED TABLE OF ts_parse_element_level WITH UNIQUE KEY level.

    " Constants IXML_MR and IXML_IID from the type pool IXML 7.52
    CONSTANTS: BEGIN OF ixml_mr,
                 ok                    TYPE i VALUE 0,
                 dom_ok                TYPE i VALUE 0,
                 dom_unspecified       TYPE i VALUE 131073,
                 dom_internal          TYPE i VALUE 131074,
                 dom_invalid_arg       TYPE i VALUE 131075,
                 dom_index_size        TYPE i VALUE 131076,
                 dom_string_size       TYPE i VALUE 131077,
                 dom_hierarchy_request TYPE i VALUE 131078,
                 dom_wrong_document    TYPE i VALUE 131079,
                 dom_not_allowed       TYPE i VALUE 131080,
                 dom_no_mod_allowed    TYPE i VALUE 131081,
                 dom_not_found         TYPE i VALUE 131082,
                 dom_not_supported     TYPE i VALUE 131083,
                 dom_is_use_attribute  TYPE i VALUE 131084,
                 dom_parent_recursion  TYPE i VALUE 131085,
                 dom_not_parent_child  TYPE i VALUE 131086,
                 ixml_ok               TYPE i VALUE 0,
                 ixml_unspecified      TYPE i VALUE 65537,
                 ixml_internal         TYPE i VALUE 65538,
                 ixml_invalid_arg      TYPE i VALUE 65539,
                 ixml_invalidi_xml     TYPE i VALUE 65540,
                 ixml_invalid_did      TYPE i VALUE 65541,
                 ixml_invalid_this     TYPE i VALUE 65542,
                 ixml_invalid_hash     TYPE i VALUE 65543,
                 parser_ok             TYPE i VALUE 0,
                 parser_unspecified    TYPE i VALUE 196609,
                 parser_internal       TYPE i VALUE 196610,
                 parser_invalid_arg    TYPE i VALUE 196611,
                 parser_error          TYPE i VALUE 196612,
                 parser_fatal_error    TYPE i VALUE 196613,
                 renderer_ok           TYPE i VALUE 0,
                 renderer_unspecified  TYPE i VALUE 262145,
                 renderer_internal     TYPE i VALUE 262146,
                 renderer_invalid_arg  TYPE i VALUE 262147,
                 renderer_error        TYPE i VALUE 262148,
                 renderer_fatal_error  TYPE i VALUE 262149,
                 io_ok                 TYPE i VALUE 0,
                 io_unspecified        TYPE i VALUE 327681,
                 io_internal           TYPE i VALUE 327682,
                 io_invalid_arg        TYPE i VALUE 327683,
                 io_conv_problem       TYPE i VALUE 327684,
               END OF ixml_mr.

    " ------------------------------------------------------------------
    " interface IDs
    " ------------------------------------------------------------------

    CONSTANTS: BEGIN OF ixml_iid,
                 unknown               TYPE i VALUE 1,
                 ixml                  TYPE i VALUE 10,
                 node                  TYPE i VALUE 20,
                 node_list             TYPE i VALUE 30,
                 named_node_map        TYPE i VALUE 40,
                 node_collection       TYPE i VALUE 50,
                 node_iterator         TYPE i VALUE 60,
                 inline_iterator       TYPE i VALUE 61,
                 node_filter           TYPE i VALUE 70,
                 node_filter_combining TYPE i VALUE 80,
                 document_fragment     TYPE i VALUE 90,
                 document              TYPE i VALUE 100,
                 character_data        TYPE i VALUE 110,
                 attribute             TYPE i VALUE 120,
                 element               TYPE i VALUE 130,
                 text                  TYPE i VALUE 140,
                 comment               TYPE i VALUE 150,
                 pi                    TYPE i VALUE 160,
                 pi_unparsed           TYPE i VALUE 170,
                 pi_parsed             TYPE i VALUE 180,
                 cdata_section         TYPE i VALUE 190,
                 entity_ref            TYPE i VALUE 200,
                 document_type         TYPE i VALUE 210,
                 cond_dtd_section      TYPE i VALUE 220,
                 element_decl          TYPE i VALUE 230,
                 content_particle      TYPE i VALUE 240,
                 att_list_decl         TYPE i VALUE 250,
                 attribute_decl        TYPE i VALUE 260,
                 entity_decl           TYPE i VALUE 270,
                 notation_decl         TYPE i VALUE 280,
                 namespace_decl        TYPE i VALUE 281,
                 namespace_context     TYPE i VALUE 282,
                 encoding              TYPE i VALUE 290,
                 event                 TYPE i VALUE 310,
                 parser                TYPE i VALUE 320,
                 parse_error           TYPE i VALUE 330,
                 token_parser          TYPE i VALUE 331,
                 renderer              TYPE i VALUE 340,
                 token_renderer        TYPE i VALUE 341,
                 stream_factory        TYPE i VALUE 350,
                 stream                TYPE i VALUE 360,
                 istream               TYPE i VALUE 370,
                 ostream               TYPE i VALUE 380,
               END OF ixml_iid.

  PRIVATE SECTION.

    CLASS-METHODS get_singleton
      RETURNING
        VALUE(rval) TYPE REF TO lcl_ixml_factory.

    CLASS-DATA singleton TYPE REF TO lcl_ixml_factory.

ENDCLASS.
