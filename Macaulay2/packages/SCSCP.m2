f = openInOut "$192.168.1.9:26133"
while not isReady f do nothing
sleep 1
read f
f << ///<?scscp version="1.3" ?>/// << endl << flush
while not isReady f do nothing
sleep 1
read f
f << ///
  <?scscp start ?>
  <OMOBJ>
  <OMATTR>
  <OMATP>
    <OMS cd="scscp1" name="call_id"/>
    <OMSTR>hi there</OMSTR>
    <OMS cd="scscp1" name="option_return_object"/>
    <OMSTR></OMSTR>
  </OMATP>
    <OMA>
      <OMS cd="scscp1" name="procedure_call"/>
      <OMA>
	<OMS cd="arith1" name="plus"/>
	<OMI>1</OMI>
	<OMI>2</OMI>
      </OMA>
    </OMA>
  </OMATTR>
  </OMOBJ>
  <?scscp end ?>
  /// << endl << flush


to do:

  Dan G:
       install GAP kernel and packages
  
  Dan R:
  
  