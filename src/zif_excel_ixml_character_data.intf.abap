INTERFACE zif_excel_ixml_character_data
  PUBLIC.

  INTERFACES zif_excel_ixml_node.

  ALIASES append_child
    FOR zif_excel_ixml_node~append_child.
  ALIASES clone
    FOR zif_excel_ixml_node~clone.
  ALIASES create_iterator
    FOR zif_excel_ixml_node~create_iterator.
  ALIASES get_attributes
    FOR zif_excel_ixml_node~get_attributes.
  ALIASES get_children
    FOR zif_excel_ixml_node~get_children.
  ALIASES get_first_child
    FOR zif_excel_ixml_node~get_first_child.
  ALIASES get_name
    FOR zif_excel_ixml_node~get_name.
  ALIASES get_next
    FOR zif_excel_ixml_node~get_next.
  ALIASES get_value
    FOR zif_excel_ixml_node~get_value.
  ALIASES set_value
    FOR zif_excel_ixml_node~set_value.

ENDINTERFACE.
