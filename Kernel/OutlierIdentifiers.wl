
BeginPackage["AntonAntonov`OutlierIdentifiers`"];

HampelIdentifierParameters::usage = "Returns Hampel outlier identifier parameters {L,U} for a list of numbers.";

QuartileIdentifierParameters::usage = "Returns quartile outlier identifier parameters {L,U} for a list of numbers.";

SPLUSQuartileIdentifierParameters::usage = "Returns SPLUS quartile outlier identifier parameters {L,U} for a list of numbers.";

OutlierIdentify::usage = "OutlierIdentify[data : {_?NumberQ...} | Association[ (_ -> _?NumberQ) ..], pars] \
applies outlier identifier parameters pars to a list of numbers dataArg.";

OutlierIdentifier::usage = "Synonym of OutlierIdentify.";

OutlierIdentifyLess::usage = "OutlierIdentifyLess[ data : {_?NumberQ...} | Association[ (_ -> _?NumberQ) ..], pars] \
applies outlier identifier parameters pars to a list of numbers data and takes the outliers with smallest values.";

TopOutliers::usage = "Changes the parameters {L,U} of an outlier identifier to {-Infinity,U}.";

BottomOutliers::usage = "Changes the parameters {L,U} of an outlier identifier to {L,Infinity}.";

HampelIdentifier::usage = "Shortcut for OutlierIdentify[#, HampelIdentifierParameters]& .";

QuartileIdentifier::usage = "Shortcut for OutlierIdentify[#, QuartileIdentifierParameters]& .";

SPLUSQuartileIdentifier::usage = "Shortcut for OutlierIdentify[#, SPLUSQuartileIdentifierParameters]& .";

OutlierPosition::usage = "OutlierPosition[ data : {_?NumberQ...}, pars] gives the positions of the outliers \
in data using the outlier identifier parameters pars. Top and bottom outliers can be found with
TopOutliers @* pars and BottomOutliers @* pars respectively.";

ListPlotOutliers::usage = "Plots a list of numbers and its outliers using ListPlot.";

ColorPlotOutliers::usage = "ColorPlotOutliers[oid___] makes a function for coloring the outliers in list point plots.";

Begin["`Private`"];

Clear[HampelIdentifierParameters];
HampelIdentifierParameters[data : {_?NumberQ...}] :=
    Block[{x0 = Median[data], md},
      md = 1.4826 * Median[Abs[data - x0]];
      {x0 - md, x0 + md}
    ];


Clear[QuartileIdentifierParameters];
QuartileIdentifierParameters[data : {_?NumberQ...}] :=
    Block[{xL, xU, x0},
      {xL, x0, xU} = Quantile[data, {1 / 4, 1 / 2, 3 / 4}];
      {x0 - (xU - xL), x0 + (xU - xL)}
    ];


Clear[SPLUSQuartileIdentifierParameters];
SPLUSQuartileIdentifierParameters[data : {_?NumberQ...}] :=
    Block[{xL, xU},
      If[Length[data] <= 4, Return[{Min[data], Max[data]}]];
      {xL, xU} = Quantile[data, {1 / 4, 3 / 4}];
      {xL - 1.5 * (xU - xL), xU + 1.5 * (xU - xL)}
    ];


Clear[TopOutliers, BottomOutliers];
TopOutliers[{xL_, xU_}] := {-Infinity, xU};
BottomOutliers[{xL_, xU_}] := {xL, Infinity};


(***********::Section:: ***********)
(* Identifiers                    *)
(**********************************)

Clear[OutlierIdentify, OutlierIdentifyLess];
OutlierIdentify[data : {_?NumberQ...}, outlierIdentifierParameters_ : HampelIdentifierParameters ] :=
    Block[{xL, xU},
      {xL, xU} = outlierIdentifierParameters[data];
      Select[data, # < xL || xU < #&]
    ];

OutlierIdentify[ data : Association[ (_ -> _?NumberQ) ..], outlierIdentifierParameters_ : HampelIdentifierParameters ] :=
    KeyTake[ data, OutlierPosition[data, outlierIdentifierParameters] ];

OutlierIdentifyLess[data : {_?NumberQ...} | Association[ (_ -> _?NumberQ) ..], outlierIdentifierParameters_ : HampelIdentifierParameters ] :=
    OutlierIdentify[ data, BottomOutliers @* outlierIdentifierParameters ];

Clear[OutlierIdentifier];
OutlierIdentifier = OutlierIdentify;

Clear[HampelIdentifier];
HampelIdentifier[data_] := OutlierIdentify[data, HampelIdentifierParameters];

Clear[QuartileIdentifier];
QuartileIdentifier[data_] := OutlierIdentify[data, QuartileIdentifierParameters];

Clear[SPLUSQuartileIdentifier];
SPLUSQuartileIdentifier[data_] := OutlierIdentify[data, SPLUSQuartileIdentifierParameters];


Clear[OutlierPosition];
OutlierPosition[data : {_?NumberQ...}, outlierIdentifier_ : HampelIdentifierParameters] :=
    Block[{cls, t},
      cls = OutlierIdentify[data, outlierIdentifier];
      t = Select[Transpose[{data, Range[Length[data]]}], MemberQ[cls, #[[1]]]&];
      If[t === {}, {}, t[[All, 2]]]
    ];

OutlierPosition[data : Association[ (_ -> _?NumberQ) ... ], outlierIdentifier_ : HampelIdentifierParameters ] :=
    Block[{pos},
      pos = OutlierPosition[ Values[data], outlierIdentifier];
      If[ pos === {}, {}, Keys[data][[pos]] ]
    ];


(*********** ::Section:: ***********)
(* Plot definitions                *)
(***********************************)

Clear[ListPlotOutliers];
Options[ListPlotOutliers] = {PlotStyle -> {PointSize[0.015]}, PlotRange -> All, ImageSize -> 300};
ListPlotOutliers[ds_, outlierParameters_, optsArg___] :=
    Block[{outliers, opts = optsArg, positionedOutliers},
      If[!OptionQ[{opts}], opts = Options[ListPlotOutliers]];
      outliers = OutlierIdentify[ds, outlierParameters];
      If[outliers === {},
        ListPlot[Transpose[{Range[Length[ds]], ds}], opts],
        positionedOutliers = Select[Transpose[{Range[Length[ds]], ds}], MemberQ[outliers, #[[2]]]&];
        ListPlot[{Transpose[{Range[Length[ds]], ds}], positionedOutliers}, opts]
      ]
    ];

ClearAll[ColorPlotOutliers];
ColorPlotOutliers[] := # /. {Point[ps_] :> {Point[ps], Red, Point[ps[[OutlierPosition[ps[[All, 2]]]]]]}} &;
ColorPlotOutliers[oid_] := # /. {Point[ps_] :> {Point[ps], Red, Point[ps[[OutlierPosition[ps[[All, 2]], oid]]]]}} &;

End[];
EndPackage[];