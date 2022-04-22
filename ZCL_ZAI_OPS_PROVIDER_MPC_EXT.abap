class ZCL_ZAI_OPS_PROVIDER_MPC_EXT definition
  public
  inheriting from ZCL_ZAI_OPS_PROVIDER_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZAI_OPS_PROVIDER_MPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZAI_OPS_PROVIDER_MPC_EXT->DEFINE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] /IWBEP/CX_MGW_MED_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD define.

    super->define( ).
    DATA: lo_entity   TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property TYPE REF TO /iwbep/if_mgw_odata_property.
    lo_entity = model->get_entity_type( iv_entity_name = 'mai_snapshot_request' ).
    IF lo_entity IS BOUND.
      lo_property = lo_entity->get_property( iv_property_name = 'FileName').
      lo_property->set_as_content_type( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.