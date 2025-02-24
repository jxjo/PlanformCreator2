## Export to Xflr5

When using PC2 together with Xlfr5 the major faciliation beside the definition of wing segements, is the automatic generation of all intermediate airfoils needed in Xflr5 at all wing sections. No further geometric work has to be done for wing definition and airfoils. The steps to perform wing analysis in Xflr5 together with PC2:

#### Actions needed:

| | App      | Module       |  Action        |  
| :---| :---         |     :---        |   :---         |  
|1 | PC2   | Wing Analysis | Export to Xflr5 the xml wing definition and all airfoils involved to a subdirectory      |
|2 | Xflr5   | Direct Foil Design     | Open the airfoils in a single step |  
|3 | Xflr5   | Xfoil Direct Analysis  | Batch Analysis of all the airfoils with a polarset covering the complete Reynolds range      |
|4 | Xflr5  |Wing and Plane Design  |Create a new plane    |
|5 | Xflr5  |Wing and Plane Design - Edit Wing |Menu button 'Other' and select 'Import Wing from xml file'   |
|6 | Xflr5  |Wing and Plane Design  |Ready for calculation. Don't forget to define a 'Plane Mass' for T2 analysis.  |

#### Importing wing xml into Xflr5

<img src="../images/panelling_xflr5_loaded.png" width="800" />

