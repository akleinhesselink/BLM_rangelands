<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis maxScale="0" styleCategories="AllStyleCategories" minScale="1e+08" version="3.18.0-Zürich" hasScaleBasedVisibilityFlag="0">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal mode="0" fetchMode="0" enabled="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <property value="false" key="WMSBackgroundLayer"/>
    <property value="false" key="WMSPublishDataSourceUrl"/>
    <property value="0" key="embeddedWidgets/count"/>
    <property value="Value" key="identify/format"/>
  </customproperties>
  <pipe>
    <provider>
      <resampling maxOversampling="2" zoomedInResamplingMethod="nearestNeighbour" enabled="false" zoomedOutResamplingMethod="nearestNeighbour"/>
    </provider>
    <rasterrenderer nodataColor="" classificationMin="-0.0037092" opacity="1" band="1" classificationMax="0.0478174" type="singlebandpseudocolor" alphaBand="-1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>MinMax</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader maximumValue="0.047817400000000003" clip="0" colorRampType="INTERPOLATED" classificationMode="1" minimumValue="-0.0037092000000000002" labelPrecision="4">
          <colorramp name="[source]" type="gradient">
            <Option type="Map">
              <Option value="68,1,84,255" name="color1" type="QString"/>
              <Option value="253,231,37,255" name="color2" type="QString"/>
              <Option value="0" name="discrete" type="QString"/>
              <Option value="gradient" name="rampType" type="QString"/>
              <Option value="0.0196078;70,8,92,255:0.0392157;71,16,99,255:0.0588235;72,23,105,255:0.0784314;72,29,111,255:0.0980392;72,36,117,255:0.117647;71,42,122,255:0.137255;70,48,126,255:0.156863;69,55,129,255:0.176471;67,61,132,255:0.196078;65,66,135,255:0.215686;63,72,137,255:0.235294;61,78,138,255:0.254902;58,83,139,255:0.27451;56,89,140,255:0.294118;53,94,141,255:0.313725;51,99,141,255:0.333333;49,104,142,255:0.352941;46,109,142,255:0.372549;44,113,142,255:0.392157;42,118,142,255:0.411765;41,123,142,255:0.431373;39,128,142,255:0.45098;37,132,142,255:0.470588;35,137,142,255:0.490196;33,142,141,255:0.509804;32,146,140,255:0.529412;31,151,139,255:0.54902;30,156,137,255:0.568627;31,161,136,255:0.588235;33,165,133,255:0.607843;36,170,131,255:0.627451;40,174,128,255:0.647059;46,179,124,255:0.666667;53,183,121,255:0.686275;61,188,116,255:0.705882;70,192,111,255:0.72549;80,196,106,255:0.745098;90,200,100,255:0.764706;101,203,94,255:0.784314;112,207,87,255:0.803922;124,210,80,255:0.823529;137,213,72,255:0.843137;149,216,64,255:0.862745;162,218,55,255:0.882353;176,221,47,255:0.901961;189,223,38,255:0.921569;202,225,31,255:0.941176;216,226,25,255:0.960784;229,228,25,255:0.980392;241,229,29,255" name="stops" type="QString"/>
            </Option>
            <prop k="color1" v="68,1,84,255"/>
            <prop k="color2" v="253,231,37,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.0196078;70,8,92,255:0.0392157;71,16,99,255:0.0588235;72,23,105,255:0.0784314;72,29,111,255:0.0980392;72,36,117,255:0.117647;71,42,122,255:0.137255;70,48,126,255:0.156863;69,55,129,255:0.176471;67,61,132,255:0.196078;65,66,135,255:0.215686;63,72,137,255:0.235294;61,78,138,255:0.254902;58,83,139,255:0.27451;56,89,140,255:0.294118;53,94,141,255:0.313725;51,99,141,255:0.333333;49,104,142,255:0.352941;46,109,142,255:0.372549;44,113,142,255:0.392157;42,118,142,255:0.411765;41,123,142,255:0.431373;39,128,142,255:0.45098;37,132,142,255:0.470588;35,137,142,255:0.490196;33,142,141,255:0.509804;32,146,140,255:0.529412;31,151,139,255:0.54902;30,156,137,255:0.568627;31,161,136,255:0.588235;33,165,133,255:0.607843;36,170,131,255:0.627451;40,174,128,255:0.647059;46,179,124,255:0.666667;53,183,121,255:0.686275;61,188,116,255:0.705882;70,192,111,255:0.72549;80,196,106,255:0.745098;90,200,100,255:0.764706;101,203,94,255:0.784314;112,207,87,255:0.803922;124,210,80,255:0.823529;137,213,72,255:0.843137;149,216,64,255:0.862745;162,218,55,255:0.882353;176,221,47,255:0.901961;189,223,38,255:0.921569;202,225,31,255:0.941176;216,226,25,255:0.960784;229,228,25,255:0.980392;241,229,29,255"/>
          </colorramp>
          <item value="-0.0037092" label="-0.0037" color="#440154" alpha="255"/>
          <item value="-0.00269887673252" label="-0.0027" color="#46085c" alpha="255"/>
          <item value="-0.00168854831238" label="-0.0017" color="#471063" alpha="255"/>
          <item value="-0.0006782250449" label="-0.0007" color="#481769" alpha="255"/>
          <item value="0.00033210337524" label="0.0003" color="#481d6f" alpha="255"/>
          <item value="0.00134242664272" label="0.0013" color="#482475" alpha="255"/>
          <item value="0.0023527499102" label="0.0024" color="#472a7a" alpha="255"/>
          <item value="0.003363083483" label="0.0034" color="#46307e" alpha="255"/>
          <item value="0.0043734170558" label="0.0044" color="#453781" alpha="255"/>
          <item value="0.0053837506286" label="0.0054" color="#433d84" alpha="255"/>
          <item value="0.0063940326748" label="0.0064" color="#414287" alpha="255"/>
          <item value="0.0074043662476" label="0.0074" color="#3f4889" alpha="255"/>
          <item value="0.0084146998204" label="0.0084" color="#3d4e8a" alpha="255"/>
          <item value="0.0094250333932" label="0.0094" color="#3a538b" alpha="255"/>
          <item value="0.010435366966" label="0.0104" color="#38598c" alpha="255"/>
          <item value="0.0114457005388" label="0.0114" color="#355e8d" alpha="255"/>
          <item value="0.012455982585" label="0.0125" color="#33638d" alpha="255"/>
          <item value="0.0134663161578" label="0.0135" color="#31688e" alpha="255"/>
          <item value="0.0144766497306" label="0.0145" color="#2e6d8e" alpha="255"/>
          <item value="0.0154869833034" label="0.0155" color="#2c718e" alpha="255"/>
          <item value="0.0164973168762" label="0.0165" color="#2a768e" alpha="255"/>
          <item value="0.017507650449" label="0.0175" color="#297b8e" alpha="255"/>
          <item value="0.0185179840218" label="0.0185" color="#27808e" alpha="255"/>
          <item value="0.019528266068" label="0.0195" color="#25848e" alpha="255"/>
          <item value="0.0205385996408" label="0.0205" color="#23898e" alpha="255"/>
          <item value="0.0215489332136" label="0.0215" color="#218e8d" alpha="255"/>
          <item value="0.0225592667864" label="0.0226" color="#20928c" alpha="255"/>
          <item value="0.0235696003592" label="0.0236" color="#1f978b" alpha="255"/>
          <item value="0.024579933932" label="0.0246" color="#1e9c89" alpha="255"/>
          <item value="0.0255902159782" label="0.0256" color="#1fa188" alpha="255"/>
          <item value="0.026600549551" label="0.0266" color="#21a585" alpha="255"/>
          <item value="0.0276108831238" label="0.0276" color="#24aa83" alpha="255"/>
          <item value="0.0286212166966" label="0.0286" color="#28ae80" alpha="255"/>
          <item value="0.0296315502694" label="0.0296" color="#2eb37c" alpha="255"/>
          <item value="0.0306418838422" label="0.0306" color="#35b779" alpha="255"/>
          <item value="0.031652217415" label="0.0317" color="#3dbc74" alpha="255"/>
          <item value="0.0326624994612" label="0.0327" color="#46c06f" alpha="255"/>
          <item value="0.033672833034" label="0.0337" color="#50c46a" alpha="255"/>
          <item value="0.0346831666068" label="0.0347" color="#5ac864" alpha="255"/>
          <item value="0.0356935001796" label="0.0357" color="#65cb5e" alpha="255"/>
          <item value="0.0367038337524" label="0.0367" color="#70cf57" alpha="255"/>
          <item value="0.0377141673252" label="0.0377" color="#7cd250" alpha="255"/>
          <item value="0.0387244493714" label="0.0387" color="#89d548" alpha="255"/>
          <item value="0.0397347829442" label="0.0397" color="#95d840" alpha="255"/>
          <item value="0.040745116517" label="0.0407" color="#a2da37" alpha="255"/>
          <item value="0.0417554500898" label="0.0418" color="#b0dd2f" alpha="255"/>
          <item value="0.0427657836626" label="0.0428" color="#bddf26" alpha="255"/>
          <item value="0.0437761172354" label="0.0438" color="#cae11f" alpha="255"/>
          <item value="0.0447863992816" label="0.0448" color="#d8e219" alpha="255"/>
          <item value="0.0457967328544" label="0.0458" color="#e5e419" alpha="255"/>
          <item value="0.0468070664272" label="0.0468" color="#f1e51d" alpha="255"/>
          <item value="0.0478174" label="0.0478" color="#fde725" alpha="255"/>
          <rampLegendSettings orientation="2" direction="0" minimumLabel="" suffix="" prefix="" maximumLabel="">
            <numericFormat id="basic">
              <Option type="Map">
                <Option value="" name="decimal_separator" type="QChar"/>
                <Option value="6" name="decimals" type="int"/>
                <Option value="0" name="rounding_type" type="int"/>
                <Option value="false" name="show_plus" type="bool"/>
                <Option value="true" name="show_thousand_separator" type="bool"/>
                <Option value="false" name="show_trailing_zeros" type="bool"/>
                <Option value="" name="thousand_separator" type="QChar"/>
              </Option>
            </numericFormat>
          </rampLegendSettings>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" gamma="1" contrast="0"/>
    <huesaturation colorizeOn="0" colorizeStrength="100" saturation="0" grayscaleMode="0" colorizeBlue="128" colorizeGreen="128" colorizeRed="255"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
