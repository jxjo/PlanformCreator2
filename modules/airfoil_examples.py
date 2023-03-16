#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Airfoil examples 

- Root airfoil
- Tip airfoil 

"""

import io
from common_utils import * 
from airfoil import Airfoil

class Example_Airfoil (Airfoil): 

    isExample = True

    def __init__(self):
        # super().__init__()
        """Abstract superclass for exmaple airfoils
        """
        self.pathFileName = None

    @property
    def isExisting (self):
        return True

    def _getCoordinates (self):
        """ returns the coordinates of exmaple as a list of lines"""
        # to over load by sublass for specific airfoil data
        return []

    def load(self):
        # over loaded to read airfoil from code  and not from file
        file_lines = self._getCoordinates ()
        self._loadLines (file_lines)

        # tmp ? 
        self.pathFileName = self.name + '.dat'


class Root_Example (Example_Airfoil):
    """ An example airfoil for root"""

    name = 'JX-GT-15v2'

    def _getCoordinates (self):
        """ returns the coordinates of exmaple as a list of lines"""

        output = io.StringIO()
        output.write (self.name)
        output.write ("""
    1.0000000   0.0001500
    0.9901070   0.0014302
    0.9787168   0.0028980
    0.9673338   0.0043574
    0.9559528   0.0058089
    0.9445720   0.0072525
    0.9331911   0.0086880
    0.9218097   0.0101149
    0.9104277   0.0115328
    0.8990451   0.0129413
    0.8876618   0.0143397
    0.8762780   0.0157275
    0.8648938   0.0171037
    0.8535092   0.0184678
    0.8421241   0.0198187
    0.8307386   0.0211558
    0.8193526   0.0224781
    0.8079662   0.0237847
    0.7965796   0.0250747
    0.7851927   0.0263471
    0.7738056   0.0276009
    0.7624181   0.0288351
    0.7510304   0.0300489
    0.7396427   0.0312411
    0.7282550   0.0324109
    0.7168672   0.0335572
    0.7054793   0.0346792
    0.6940917   0.0357759
    0.6827044   0.0368465
    0.6713170   0.0378901
    0.6599298   0.0389062
    0.6485432   0.0398939
    0.6371572   0.0408527
    0.6257715   0.0417819
    0.6143864   0.0426812
    0.6030021   0.0435501
    0.5916186   0.0443884
    0.5802363   0.0451958
    0.5688551   0.0459720
    0.5574749   0.0467169
    0.5460962   0.0474305
    0.5347192   0.0481126
    0.5233438   0.0487632
    0.5119702   0.0493821
    0.5005987   0.0499693
    0.4892294   0.0505246
    0.4778627   0.0510478
    0.4664989   0.0515384
    0.4551385   0.0519962
    0.4437815   0.0524203
    0.4324275   0.0528103
    0.4210778   0.0531656
    0.4097331   0.0534850
    0.3983927   0.0537676
    0.3870578   0.0540124
    0.3757289   0.0542179
    0.3644064   0.0543830
    0.3530910   0.0545061
    0.3417832   0.0545857
    0.3304842   0.0546202
    0.3191946   0.0546078
    0.3079151   0.0545467
    0.2966471   0.0544352
    0.2853921   0.0542711
    0.2741509   0.0540521
    0.2629255   0.0537760
    0.2517172   0.0534404
    0.2405280   0.0530425
    0.2293612   0.0525795
    0.2182181   0.0520477
    0.2071026   0.0514439
    0.1960177   0.0507638
    0.1849681   0.0500032
    0.1739587   0.0491566
    0.1629951   0.0482187
    0.1520846   0.0471830
    0.1412353   0.0460423
    0.1304585   0.0447888
    0.1197664   0.0434132
    0.1091755   0.0419060
    0.0987071   0.0402563
    0.0883884   0.0384527
    0.0782559   0.0364834
    0.0683614   0.0343385
    0.0587781   0.0320122
    0.0496145   0.0295098
    0.0410242   0.0268570
    0.0332054   0.0241140
    0.0263595   0.0213754
    0.0206069   0.0187475
    0.0159294   0.0163084
    0.0121942   0.0140873
    0.0092269   0.0120758
    0.0068668   0.0102474
    0.0049866   0.0085730
    0.0034930   0.0070274
    0.0023195   0.0055912
    0.0014197   0.0042517
    0.0007608   0.0030020
    0.0003181   0.0018426
    0.0000724   0.0007764
    0.0000000   0.0000000
    0.0001783  -0.0010149
    0.0006845  -0.0019135
    0.0015039  -0.0028110
    0.0026105  -0.0036958
    0.0039956  -0.0045714
    0.0056793  -0.0054507
    0.0077117  -0.0063507
    0.0101789  -0.0072905
    0.0132137  -0.0082919
    0.0170098  -0.0093784
    0.0218203  -0.0105684
    0.0279085  -0.0118608
    0.0354020  -0.0132157
    0.0441531  -0.0145562
    0.0538150  -0.0158088
    0.0640491  -0.0169341
    0.0746288  -0.0179238
    0.0854230  -0.0187844
    0.0963607  -0.0195281
    0.1073990  -0.0201670
    0.1185130  -0.0207126
    0.1296846  -0.0211747
    0.1409027  -0.0215619
    0.1521582  -0.0218818
    0.1634439  -0.0221407
    0.1747556  -0.0223443
    0.1860885  -0.0224973
    0.1974400  -0.0226042
    0.2088074  -0.0226687
    0.2201888  -0.0226944
    0.2315813  -0.0226839
    0.2429847  -0.0226399
    0.2543978  -0.0225651
    0.2658189  -0.0224615
    0.2772472  -0.0223314
    0.2886822  -0.0221765
    0.3001231  -0.0219987
    0.3115694  -0.0217996
    0.3230211  -0.0215813
    0.3344768  -0.0213452
    0.3459369  -0.0210929
    0.3574006  -0.0208261
    0.3688676  -0.0205460
    0.3803383  -0.0202543
    0.3918114  -0.0199524
    0.4032876  -0.0196413
    0.4147666  -0.0193224
    0.4262475  -0.0189967
    0.4377310  -0.0186651
    0.4492170  -0.0183283
    0.4607047  -0.0179872
    0.4721941  -0.0176421
    0.4836857  -0.0172933
    0.4951790  -0.0169412
    0.5066739  -0.0165859
    0.5181703  -0.0162275
    0.5296684  -0.0158658
    0.5411680  -0.0155008
    0.5526688  -0.0151325
    0.5641712  -0.0147607
    0.5756749  -0.0143852
    0.5871802  -0.0140062
    0.5986866  -0.0136236
    0.6101940  -0.0132374
    0.6217027  -0.0128476
    0.6332128  -0.0124544
    0.6447242  -0.0120581
    0.6562370  -0.0116589
    0.6677507  -0.0112574
    0.6792652  -0.0108536
    0.6907816  -0.0104479
    0.7022990  -0.0100410
    0.7138174  -0.0096330
    0.7253373  -0.0092245
    0.7368576  -0.0088159
    0.7483784  -0.0084074
    0.7598988  -0.0079995
    0.7714182  -0.0075926
    0.7829364  -0.0071869
    0.7944530  -0.0067828
    0.8059689  -0.0063807
    0.8174836  -0.0059806
    0.8289964  -0.0055828
    0.8405079  -0.0051878
    0.8520183  -0.0047957
    0.8635271  -0.0044067
    0.8750342  -0.0040213
    0.8865401  -0.0036396
    0.8980445  -0.0032620
    0.9095467  -0.0028888
    0.9210474  -0.0025205
    0.9325466  -0.0021573
    0.9440438  -0.0017995
    0.9555392  -0.0014476
    0.9670330  -0.0011020
    0.9785262  -0.0007631
    0.9900226  -0.0004315
    1.0000000  -0.0001500""")
        
        output.seek(0)
        file_lines = output.readlines()
        output.close()
        return file_lines


class Tip_Example (Example_Airfoil):
    """ An example airfoil for tip"""

    name = 'JX-GT-05v2'

    def _getCoordinates (self):
        """ returns the coordinates of exmaple as a list of lines"""

        output = io.StringIO()
        output.write (self.name)
        output.write ("""
   1.0000000   0.0001497
   0.9902963   0.0010598
   0.9791056   0.0021079
   0.9679175   0.0031535
   0.9567303   0.0041964
   0.9455434   0.0052365
   0.9343567   0.0062737
   0.9231704   0.0073078
   0.9119843   0.0083386
   0.9007985   0.0093662
   0.8896128   0.0103903
   0.8784273   0.0114110
   0.8672425   0.0124281
   0.8560581   0.0134413
   0.8448738   0.0144506
   0.8336896   0.0154559
   0.8225059   0.0164571
   0.8113227   0.0174540
   0.8001398   0.0184465
   0.7889572   0.0194344
   0.7777752   0.0204175
   0.7665936   0.0213957
   0.7554124   0.0223687
   0.7442315   0.0233364
   0.7330511   0.0242985
   0.7218714   0.0252549
   0.7106925   0.0262052
   0.6995140   0.0271491
   0.6883360   0.0280864
   0.6771589   0.0290167
   0.6659825   0.0299397
   0.6548068   0.0308549
   0.6436318   0.0317621
   0.6324578   0.0326607
   0.6212848   0.0335503
   0.6101128   0.0344303
   0.5989419   0.0353002
   0.5877722   0.0361595
   0.5766039   0.0370074
   0.5654370   0.0378433
   0.5542716   0.0386665
   0.5431076   0.0394761
   0.5319456   0.0402714
   0.5207858   0.0410514
   0.5096279   0.0418150
   0.4984723   0.0425615
   0.4873195   0.0432895
   0.4761693   0.0439979
   0.4650223   0.0446855
   0.4538790   0.0453509
   0.4427395   0.0459925
   0.4316039   0.0466089
   0.4204732   0.0471984
   0.4093480   0.0477591
   0.3982285   0.0482891
   0.3871154   0.0487863
   0.3760098   0.0492486
   0.3649122   0.0496734
   0.3538214   0.0500583
   0.3427365   0.0504015
   0.3316574   0.0507013
   0.3205849   0.0509560
   0.3095198   0.0511637
   0.2984629   0.0513224
   0.2874149   0.0514299
   0.2763774   0.0514840
   0.2653510   0.0514822
   0.2543375   0.0514221
   0.2433385   0.0513006
   0.2323556   0.0511146
   0.2213908   0.0508607
   0.2104470   0.0505351
   0.1995269   0.0501331
   0.1886334   0.0496500
   0.1777710   0.0490800
   0.1669445   0.0484166
   0.1561589   0.0476523
   0.1454216   0.0467784
   0.1347408   0.0457852
   0.1241261   0.0446612
   0.1135908   0.0433938
   0.1031502   0.0419684
   0.0928246   0.0403691
   0.0826397   0.0385788
   0.0726294   0.0365798
   0.0628415   0.0343558
   0.0533445   0.0318946
   0.0442418   0.0291947
   0.0356898   0.0262782
   0.0279153   0.0232127
   0.0211843   0.0201265
   0.0156830   0.0171799
   0.0113985   0.0144911
   0.0081477   0.0120950
   0.0057002   0.0099664
   0.0038593   0.0080605
   0.0024818   0.0063345
   0.0014692   0.0047551
   0.0007553   0.0033002
   0.0002944   0.0019582
   0.0000545   0.0007278
   0.0000000   0.0000000
   0.0003054  -0.0015761
   0.0010021  -0.0027315
   0.0020508  -0.0038183
   0.0034332  -0.0048555
   0.0051798  -0.0058750
   0.0073783  -0.0069102
   0.0101857  -0.0079951
   0.0138551  -0.0091633
   0.0187420  -0.0104375
   0.0252003  -0.0117994
   0.0332815  -0.0131610
   0.0425857  -0.0144064
   0.0526020  -0.0154742
   0.0629983  -0.0163618
   0.0736088  -0.0170900
   0.0843531  -0.0176830
   0.0951896  -0.0181621
   0.1060924  -0.0185442
   0.1170454  -0.0188432
   0.1280376  -0.0190702
   0.1390611  -0.0192344
   0.1501098  -0.0193428
   0.1611791  -0.0194017
   0.1722659  -0.0194161
   0.1833671  -0.0193902
   0.1944809  -0.0193275
   0.2056051  -0.0192311
   0.2167384  -0.0191035
   0.2278798  -0.0189472
   0.2390280  -0.0187640
   0.2501824  -0.0185558
   0.2613419  -0.0183242
   0.2725063  -0.0180706
   0.2836747  -0.0177964
   0.2948476  -0.0175030
   0.3060233  -0.0171915
   0.3172034  -0.0168630
   0.3283880  -0.0165192
   0.3395774  -0.0161615
   0.3507714  -0.0157917
   0.3619696  -0.0154112
   0.3731725  -0.0150217
   0.3843788  -0.0146246
   0.3955887  -0.0142212
   0.4068019  -0.0138127
   0.4180187  -0.0134005
   0.4292384  -0.0129857
   0.4404610  -0.0125694
   0.4516863  -0.0121526
   0.4629144  -0.0117362
   0.4741450  -0.0113212
   0.4853780  -0.0109081
   0.4966136  -0.0104978
   0.5078508  -0.0100909
   0.5190904  -0.0096879
   0.5303313  -0.0092894
   0.5415714  -0.0088958
   0.5528099  -0.0085075
   0.5640464  -0.0081249
   0.5752812  -0.0077482
   0.5865149  -0.0073776
   0.5977464  -0.0070133
   0.6089767  -0.0066556
   0.6202058  -0.0063045
   0.6314332  -0.0059601
   0.6426594  -0.0056228
   0.6538848  -0.0052925
   0.6651086  -0.0049694
   0.6763313  -0.0046538
   0.6875532  -0.0043459
   0.6987738  -0.0040459
   0.7099936  -0.0037541
   0.7212126  -0.0034707
   0.7324306  -0.0031960
   0.7436476  -0.0029304
   0.7548639  -0.0026742
   0.7660794  -0.0024279
   0.7772941  -0.0021919
   0.7885082  -0.0019665
   0.7997214  -0.0017521
   0.8109338  -0.0015493
   0.8221458  -0.0013584
   0.8333571  -0.0011799
   0.8445676  -0.0010140
   0.8557775  -0.0008613
   0.8669869  -0.0007219
   0.8781956  -0.0005963
   0.8894038  -0.0004846
   0.9006114  -0.0003870
   0.9118183  -0.0003038
   0.9230247  -0.0002350
   0.9342307  -0.0001806
   0.9454362  -0.0001405
   0.9566410  -0.0001147
   0.9678463  -0.0001032
   0.9790519  -0.0001056
   0.9902638  -0.0001222
   1.0000000  -0.0001497""")
        
        output.seek(0)
        file_lines = output.readlines()
        output.close()
        return file_lines



# Main program for testing -----------------------------------

if __name__ == "__main__":

    myAirfoil = Root_Example()
    myTip     = Tip_Example()

    print ("New airfoil created: ", myAirfoil)
    myAirfoil.load()
    myAirfoil.plot()
    myTip.load()
    myTip.plot()
