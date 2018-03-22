{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Prelude hiding (lines, unlines)

import           Data.Text         (Text, lines, unlines)
import           NeatInterpolation (text)
import           Test.Hspec
import           Test.QuickCheck

import Data.Attoparsec.Text (parseOnly, endOfLine, parseTest, many')

import Bio.Uniprot.Type
import Bio.Uniprot.Parser

idStr :: Text
idStr = "ID   PDCD1_HUMAN             Reviewed;         288 AA."

acStr :: Text
acStr = "AC   Q15116; O00517; Q8IX89;"

dtStr :: Text
dtStr = [text|
DT   01-NOV-1997, integrated into UniProtKB/Swiss-Prot.
DT   17-APR-2007, sequence version 3.
DT   28-FEB-2018, entry version 163.|]

deStr :: Text
deStr = [text|
DE   RecName: Full=Programmed cell death protein 1;
DE            Short=Protein PD-1;
DE            Short=hPD-1;
DE   AltName: CD_antigen=CD279;
DE   Flags: Precursor;|]

gnStr :: Text
gnStr = "GN   Name=PDCD1; Synonyms=PD1;"

osStr :: Text
osStr = "OS   Homo sapiens (Human)."

ogStr :: Text
ogStr = [text|
OG   Plasmid R6-5, Plasmid IncFII R100 (NR1), and
OG   Plasmid IncFII R1-19 (R1 drd-19).|]

ocStr :: Text
ocStr = [text|
OC   Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi;
OC   Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini;
OC   Catarrhini; Hominidae; Homo.|]

oxStr :: Text
oxStr = "OX   NCBI_TaxID=9606;"

ohStr :: Text
ohStr = "OH   NCBI_TaxID=9536; Cercopithecus hamlyni (Owl-faced monkey) (Hamlyn's monkey)."

refStr :: Text
refStr = [text|
RN   [1]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA].
RX   PubMed=7851902; DOI=10.1006/geno.1994.1562;
RA   Shinohara T., Taniwaki M., Ishida Y., Kawaich M., Honjo T.;
RT   "Structure and chromosomal localization of the human PD-1 gene
RT   (PDCD1).";
RL   Genomics 23:704-706(1994).
RN   [2]
RP   NUCLEOTIDE SEQUENCE [MRNA].
RX   PubMed=9332365; DOI=10.1016/S0378-1119(97)00260-6;
RA   Finger L.R., Pu J., Wasserman R., Vibhakar R., Louie E., Hardy R.R.,
RA   Burrows P.D., Billips L.D.;
RT   "The human PD-1 gene: complete cDNA, genomic organization, and
RT   developmentally regulated expression in B cell progenitors.";
RL   Gene 197:177-187(1997).
RN   [3]
RP   ERRATUM.
RA   Finger L.R., Pu J., Wasserman R., Vibhakar R., Louie E., Hardy R.R.,
RA   Burrows P.D., Billips L.D.;
RL   Gene 203:253-253(1997).
RN   [4]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA], AND INVOLVEMENT IN SLEB2.
RX   PubMed=12402038; DOI=10.1038/ng1020;
RA   Prokunina L., Castillejo-Lopez C., Oberg F., Gunnarsson I., Berg L.,
RA   Magnusson V., Brookes A.J., Tentler D., Kristjansdottir H.,
RA   Grondal G., Bolstad A.I., Svenungsson E., Lundberg I., Sturfelt G.,
RA   Jonssen A., Truedsson L., Lima G., Alcocer-Varela J., Jonsson R.,
RA   Gyllensten U.B., Harley J.B., Alarcon-Segovia D., Steinsson K.,
RA   Alarcon-Riquelme M.E.;
RT   "A regulatory polymorphism in PDCD1 is associated with susceptibility
RT   to systemic lupus erythematosus in humans.";
RL   Nat. Genet. 32:666-669(2002).
RN   [5]
RP   NUCLEOTIDE SEQUENCE [MRNA].
RA   He X., Xu L., Liu Y., Zeng Y.;
RT   "Cloning of PD-1 cDNA from activated peripheral leukocytes.";
RL   Submitted (FEB-2003) to the EMBL/GenBank/DDBJ databases.
RN   [6]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA].
RA   Livingston R.J., Shaffer T., McFarland I., Nguyen C.P., Stanaway I.B.,
RA   Rajkumar N., Johnson E.J., da Ponte S.H., Willa H., Ahearn M.O.,
RA   Bertucci C., Acklestad J., Carroll A., Swanson J., Gildersleeve H.I.,
RA   Nickerson D.A.;
RL   Submitted (OCT-2006) to the EMBL/GenBank/DDBJ databases.
RN   [7]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE MRNA].
RX   PubMed=14702039; DOI=10.1038/ng1285;
RA   Ota T., Suzuki Y., Nishikawa T., Otsuki T., Sugiyama T., Irie R.,
RA   Wakamatsu A., Hayashi K., Sato H., Nagai K., Kimura K., Makita H.,
RA   Sekine M., Obayashi M., Nishi T., Shibahara T., Tanaka T., Ishii S.,
RA   Yamamoto J., Saito K., Kawai Y., Isono Y., Nakamura Y., Nagahari K.,
RA   Murakami K., Yasuda T., Iwayanagi T., Wagatsuma M., Shiratori A.,
RA   Sudo H., Hosoiri T., Kaku Y., Kodaira H., Kondo H., Sugawara M.,
RA   Takahashi M., Kanda K., Yokoi T., Furuya T., Kikkawa E., Omura Y.,
RA   Abe K., Kamihara K., Katsuta N., Sato K., Tanikawa M., Yamazaki M.,
RA   Ninomiya K., Ishibashi T., Yamashita H., Murakawa K., Fujimori K.,
RA   Tanai H., Kimata M., Watanabe M., Hiraoka S., Chiba Y., Ishida S.,
RA   Ono Y., Takiguchi S., Watanabe S., Yosida M., Hotuta T., Kusano J.,
RA   Kanehori K., Takahashi-Fujii A., Hara H., Tanase T.-O., Nomura Y.,
RA   Togiya S., Komai F., Hara R., Takeuchi K., Arita M., Imose N.,
RA   Musashino K., Yuuki H., Oshima A., Sasaki N., Aotsuka S.,
RA   Yoshikawa Y., Matsunawa H., Ichihara T., Shiohata N., Sano S.,
RA   Moriya S., Momiyama H., Satoh N., Takami S., Terashima Y., Suzuki O.,
RA   Nakagawa S., Senoh A., Mizoguchi H., Goto Y., Shimizu F., Wakebe H.,
RA   Hishigaki H., Watanabe T., Sugiyama A., Takemoto M., Kawakami B.,
RA   Yamazaki M., Watanabe K., Kumagai A., Itakura S., Fukuzumi Y.,
RA   Fujimori Y., Komiyama M., Tashiro H., Tanigami A., Fujiwara T.,
RA   Ono T., Yamada K., Fujii Y., Ozaki K., Hirao M., Ohmori Y.,
RA   Kawabata A., Hikiji T., Kobatake N., Inagaki H., Ikema Y., Okamoto S.,
RA   Okitani R., Kawakami T., Noguchi S., Itoh T., Shigeta K., Senba T.,
RA   Matsumura K., Nakajima Y., Mizuno T., Morinaga M., Sasaki M.,
RA   Togashi T., Oyama M., Hata H., Watanabe M., Komatsu T.,
RA   Mizushima-Sugano J., Satoh T., Shirai Y., Takahashi Y., Nakagawa K.,
RA   Okumura K., Nagase T., Nomura N., Kikuchi H., Masuho Y., Yamashita R.,
RA   Nakai K., Yada T., Nakamura Y., Ohara O., Isogai T., Sugano S.;
RT   "Complete sequencing and characterization of 21,243 full-length human
RT   cDNAs.";
RL   Nat. Genet. 36:40-45(2004).
RN   [8]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE MRNA].
RX   PubMed=15489334; DOI=10.1101/gr.2596504;
RG   The MGC Project Team;
RT   "The status, quality, and expansion of the NIH full-length cDNA
RT   project: the Mammalian Gene Collection (MGC).";
RL   Genome Res. 14:2121-2127(2004).
RN   [9]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE GENOMIC DNA].
RA   Mural R.J., Istrail S., Sutton G., Florea L., Halpern A.L.,
RA   Mobarry C.M., Lippert R., Walenz B., Shatkay H., Dew I., Miller J.R.,
RA   Flanigan M.J., Edwards N.J., Bolanos R., Fasulo D., Halldorsson B.V.,
RA   Hannenhalli S., Turner R., Yooseph S., Lu F., Nusskern D.R.,
RA   Shue B.C., Zheng X.H., Zhong F., Delcher A.L., Huson D.H.,
RA   Kravitz S.A., Mouchard L., Reinert K., Remington K.A., Clark A.G.,
RA   Waterman M.S., Eichler E.E., Adams M.D., Hunkapiller M.W., Myers E.W.,
RA   Venter J.C.;
RL   Submitted (JUL-2005) to the EMBL/GenBank/DDBJ databases.
RN   [10]
RP   FUNCTION.
RX   PubMed=21276005; DOI=10.1111/j.1749-6632.2010.05919.x;
RA   Fife B.T., Pauken K.E.;
RT   "The role of the PD-1 pathway in autoimmunity and peripheral
RT   tolerance.";
RL   Ann. N. Y. Acad. Sci. 1217:45-59(2011).|]

ccStr :: Text
ccStr = [text|
CC   -!- FUNCTION: Inhibitory cell surface receptor involved in the
CC       regulation of T-cell function during immunity and tolerance. Upon
CC       ligand binding, inhibits T-cell effector functions in an antigen-
CC       specific manner. Possible cell death inducer, in association with
CC       other factors. {ECO:0000269|PubMed:21276005}.
CC   -!- SUBUNIT: Monomer. {ECO:0000250}.
CC   -!- INTERACTION:
CC       Q9NZQ7:CD274; NbExp=2; IntAct=EBI-4314328, EBI-4314282;
CC       Q9NZQ7-1:CD274; NbExp=2; IntAct=EBI-4314328, EBI-15686469;
CC       Q06124:PTPN11; NbExp=3; IntAct=EBI-4314328, EBI-297779;
CC   -!- SUBCELLULAR LOCATION: Membrane; Single-pass type I membrane
CC       protein.
CC   -!- DEVELOPMENTAL STAGE: Induced at programmed cell death.
CC   -!- DISEASE: Systemic lupus erythematosus 2 (SLEB2) [MIM:605218]: A
CC       chronic, relapsing, inflammatory, and often febrile multisystemic
CC       disorder of connective tissue, characterized principally by
CC       involvement of the skin, joints, kidneys and serosal membranes. It
CC       is of unknown etiology, but is thought to represent a failure of
CC       the regulatory mechanisms of the autoimmune system. The disease is
CC       marked by a wide range of system dysfunctions, an elevated
CC       erythrocyte sedimentation rate, and the formation of LE cells in
CC       the blood or bone marrow. {ECO:0000269|PubMed:12402038}.
CC       Note=Disease susceptibility is associated with variations
CC       affecting the gene represented in this entry.|]

peStr :: Text
peStr = "PE   1: Evidence at protein level;"

kwStr :: Text
kwStr = [text|
KW   3D-structure; Apoptosis; Complete proteome; Disulfide bond;
KW   Glycoprotein; Immunity; Immunoglobulin domain; Membrane; Polymorphism;
KW   Reference proteome; Signal; Systemic lupus erythematosus;
KW   Transmembrane; Transmembrane helix.|]

drStr :: Text
drStr = [text|
DR   EMBL; L27440; AAC41700.1; -; Genomic_DNA.
DR   EMBL; U64863; AAC51773.1; -; mRNA.
DR   EMBL; AF363458; AAN64003.1; -; Genomic_DNA.
DR   EMBL; AY238517; AAO63583.1; -; mRNA.
DR   EMBL; EF064716; ABK41899.1; -; Genomic_DNA.
DR   EMBL; AK313848; BAG36577.1; -; mRNA.
DR   EMBL; CH471063; EAW71298.1; -; Genomic_DNA.
DR   EMBL; BC074740; AAH74740.1; -; mRNA.
DR   CCDS; CCDS33428.1; -.
DR   PIR; A55737; A55737.
DR   RefSeq; NP_005009.2; NM_005018.2.
DR   UniGene; Hs.158297; -.
DR   PDB; 2M2D; NMR; -; A=34-150.
DR   PDB; 3RRQ; X-ray; 2.10 A; A=32-160.
DR   PDB; 4ZQK; X-ray; 2.45 A; B=33-150.
DR   PDB; 5B8C; X-ray; 2.15 A; C/F/I/L=32-160.
DR   PDB; 5GGR; X-ray; 3.30 A; Y/Z=26-150.
DR   PDB; 5GGS; X-ray; 2.00 A; Y/Z=26-148.
DR   PDB; 5IUS; X-ray; 2.89 A; A/B=26-146.
DR   PDB; 5JXE; X-ray; 2.90 A; A/B=33-146.
DR   PDB; 5WT9; X-ray; 2.40 A; G=1-167.
DR   PDBsum; 2M2D; -.
DR   PDBsum; 3RRQ; -.
DR   PDBsum; 4ZQK; -.
DR   PDBsum; 5B8C; -.
DR   PDBsum; 5GGR; -.
DR   PDBsum; 5GGS; -.
DR   PDBsum; 5IUS; -.
DR   PDBsum; 5JXE; -.
DR   PDBsum; 5WT9; -.
DR   ProteinModelPortal; Q15116; -.
DR   SMR; Q15116; -.
DR   BioGrid; 111160; 61.
DR   DIP; DIP-44126N; -.
DR   IntAct; Q15116; 5.
DR   MINT; Q15116; -.
DR   STRING; 9606.ENSP00000335062; -.
DR   ChEMBL; CHEMBL3307223; -.
DR   DrugBank; DB05916; CT-011.
DR   DrugBank; DB09035; Nivolumab.
DR   DrugBank; DB09037; Pembrolizumab.
DR   GuidetoPHARMACOLOGY; 2760; -.
DR   iPTMnet; Q15116; -.
DR   PhosphoSitePlus; Q15116; -.
DR   BioMuta; PDCD1; -.
DR   DMDM; 145559515; -.
DR   PaxDb; Q15116; -.
DR   PeptideAtlas; Q15116; -.
DR   PRIDE; Q15116; -.
DR   DNASU; 5133; -.
DR   Ensembl; ENST00000334409; ENSP00000335062; ENSG00000188389.
DR   Ensembl; ENST00000618185; ENSP00000480684; ENSG00000276977.
DR   GeneID; 5133; -.
DR   KEGG; hsa:5133; -.
DR   UCSC; uc002wcq.5; human.
DR   CTD; 5133; -.
DR   DisGeNET; 5133; -.
DR   EuPathDB; HostDB:ENSG00000188389.10; -.
DR   GeneCards; PDCD1; -.
DR   H-InvDB; HIX0030684; -.
DR   HGNC; HGNC:8760; PDCD1.
DR   HPA; CAB038418; -.
DR   HPA; HPA035981; -.
DR   MalaCards; PDCD1; -.
DR   MIM; 109100; phenotype.
DR   MIM; 600244; gene.
DR   MIM; 605218; phenotype.
DR   neXtProt; NX_Q15116; -.
DR   OpenTargets; ENSG00000188389; -.
DR   Orphanet; 802; Multiple sclerosis.
DR   Orphanet; 536; Systemic lupus erythematosus.
DR   PharmGKB; PA33110; -.
DR   eggNOG; ENOG410J26W; Eukaryota.
DR   eggNOG; ENOG41116U6; LUCA.
DR   GeneTree; ENSGT00390000013662; -.
DR   HOGENOM; HOG000253959; -.
DR   HOVERGEN; HBG053534; -.
DR   InParanoid; Q15116; -.
DR   KO; K06744; -.
DR   OMA; DFQWREK; -.
DR   OrthoDB; EOG091G0EE8; -.
DR   PhylomeDB; Q15116; -.
DR   TreeFam; TF336181; -.
DR   Reactome; R-HSA-389948; PD-1 signaling.
DR   SIGNOR; Q15116; -.
DR   ChiTaRS; PDCD1; human.
DR   GeneWiki; Programmed_cell_death_1; -.
DR   GenomeRNAi; 5133; -.
DR   PRO; PR:Q15116; -.
DR   Proteomes; UP000005640; Chromosome 2.
DR   Bgee; ENSG00000188389; -.
DR   CleanEx; HS_PDCD1; -.
DR   ExpressionAtlas; Q15116; baseline and differential.
DR   Genevisible; Q15116; HS.
DR   GO; GO:0009897; C:external side of plasma membrane; IEA:Ensembl.
DR   GO; GO:0016021; C:integral component of membrane; IEA:UniProtKB-KW.
DR   GO; GO:0005886; C:plasma membrane; TAS:Reactome.
DR   GO; GO:0004871; F:signal transducer activity; TAS:ProtInc.
DR   GO; GO:0006915; P:apoptotic process; TAS:ProtInc.
DR   GO; GO:0006959; P:humoral immune response; TAS:ProtInc.
DR   GO; GO:0007275; P:multicellular organism development; TAS:ProtInc.
DR   GO; GO:0043066; P:negative regulation of apoptotic process; IEA:Ensembl.
DR   GO; GO:0002644; P:negative regulation of tolerance induction; IEA:Ensembl.
DR   GO; GO:0070234; P:positive regulation of T cell apoptotic process; IDA:UniProtKB.
DR   GO; GO:0031295; P:T cell costimulation; TAS:Reactome.
DR   Gene3D; 2.60.40.10; -; 1.
DR   InterPro; IPR007110; Ig-like_dom.
DR   InterPro; IPR036179; Ig-like_dom_sf.
DR   InterPro; IPR013783; Ig-like_fold.
DR   InterPro; IPR003599; Ig_sub.
DR   InterPro; IPR013106; Ig_V-set.
DR   Pfam; PF07686; V-set; 1.
DR   SMART; SM00409; IG; 1.
DR   SMART; SM00406; IGv; 1.
DR   SUPFAM; SSF48726; SSF48726; 1.
DR   PROSITE; PS50835; IG_LIKE; 1.|]

ftStr :: Text
ftStr = [text|
FT   SIGNAL        1     20       {ECO:0000255}.
FT   CHAIN        21    288       Programmed cell death protein 1.
FT                                /FTId=PRO_0000014892.
FT   TOPO_DOM     21    170       Extracellular. {ECO:0000255}.
FT   TRANSMEM    171    191       Helical. {ECO:0000255}.
FT   TOPO_DOM    192    288       Cytoplasmic. {ECO:0000255}.
FT   DOMAIN       35    145       Ig-like V-type.
FT   CARBOHYD     49     49       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD     58     58       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD     74     74       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD    116    116       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   DISULFID     54    123       {ECO:0000255|PROSITE-ProRule:PRU00114}.
FT   VARIANT     215    215       A -> V (in dbSNP:rs2227982).
FT                                /FTId=VAR_031685.
FT   CONFLICT     38     38       S -> F (in Ref. 2; AAC51773).
FT                                {ECO:0000305}.
FT   CONFLICT    162    162       P -> S (in Ref. 1; AAC41700).
FT                                {ECO:0000305}.
FT   STRAND       27     29       {ECO:0000244|PDB:5WT9}.
FT   STRAND       36     38       {ECO:0000244|PDB:5GGS}.
FT   STRAND       40     45       {ECO:0000244|PDB:5GGS}.
FT   STRAND       50     55       {ECO:0000244|PDB:5GGS}.
FT   STRAND       60     70       {ECO:0000244|PDB:5GGS}.
FT   STRAND       72     74       {ECO:0000244|PDB:4ZQK}.
FT   STRAND       76     83       {ECO:0000244|PDB:5GGS}.
FT   STRAND       95     99       {ECO:0000244|PDB:5GGS}.
FT   STRAND      103    112       {ECO:0000244|PDB:5GGS}.
FT   HELIX       115    117       {ECO:0000244|PDB:5GGS}.
FT   STRAND      119    131       {ECO:0000244|PDB:5GGS}.
FT   STRAND      134    136       {ECO:0000244|PDB:5GGS}.
FT   STRAND      140    145       {ECO:0000244|PDB:5GGS}.|]
  
sqStr :: Text
sqStr = [text|
SQ   SEQUENCE   288 AA;  31647 MW;  A5210FD40C304FB7 CRC64;
     MQIPQAPWPV VWAVLQLGWR PGWFLDSPDR PWNPPTFSPA LLVVTEGDNA TFTCSFSNTS
     ESFVLNWYRM SPSNQTDKLA AFPEDRSQPG QDCRFRVTQL PNGRDFHMSV VRARRNDSGT
     YLCGAISLAP KAQIKESLRA ELRVTERRAE VPTAHPSPSP RPAGQFQTLV VGVVGGLLGS
     LVLLVWVLAV ICSRAARGTI GARRTGQPLK EDPSAVPVFS VDYGELDFQW REKTPEPPVP
     CVPEQTEYAT IVFPSGMGTS SPARRGSADG PRSAQPLRPE DGHCSWPL|]

endStr :: Text
endStr = "//"

idAns :: ID
idAns = ID "PDCD1_HUMAN" Reviewed 288

acAns :: AC
acAns = AC ["Q15116", "O00517", "Q8IX89"]

dtAns :: DT
dtAns = DT "01-NOV-1997" "Swiss-Prot" "17-APR-2007" 3 "28-FEB-2018" 163

deAns :: DE
deAns = DE (Just (Name "Programmed cell death protein 1" ["Protein PD-1", "hPD-1"] []))
           [CDAntigen "CD279"] [] [] [] (Just Precursor)

gnAns :: [GN]
gnAns = [GN (Just "PDCD1") ["PD1"] [] []]

osAns :: OS
osAns = OS "Homo sapiens (Human)"

ogAns :: OG
ogAns = Plasmid ["R6-5", "IncFII R100 (NR1)", "IncFII R1-19 (R1 drd-19)"]

ocAns :: OC
ocAns = OC ["Eukaryota", "Metazoa", "Chordata", "Craniata",
            "Vertebrata", "Euteleostomi", "Mammalia", "Eutheria",
            "Euarchontoglires", "Primates", "Haplorrhini", "Catarrhini",
            "Hominidae", "Homo"]

ohAns :: OH
ohAns = OH "9536" "Cercopithecus hamlyni (Owl-faced monkey) (Hamlyn's monkey)"

oxAns :: OX
oxAns = OX "NCBI_TaxID" "9606"

refAns :: [Reference]
refAns = undefined

ccAns :: [CC]
ccAns = undefined

drAns :: [DR]
drAns = [DR {resourceAbbr = "EMBL", resourceId = "L27440", optionalInfo = ["AAC41700.1","-","Genomic_DNA"]},
         DR {resourceAbbr = "EMBL", resourceId = "U64863", optionalInfo = ["AAC51773.1","-","mRNA"]},
         DR {resourceAbbr = "EMBL", resourceId = "AF363458", optionalInfo = ["AAN64003.1","-","Genomic_DNA"]},
         DR {resourceAbbr = "EMBL", resourceId = "AY238517", optionalInfo = ["AAO63583.1","-","mRNA"]},
         DR {resourceAbbr = "EMBL", resourceId = "EF064716", optionalInfo = ["ABK41899.1","-","Genomic_DNA"]},
         DR {resourceAbbr = "EMBL", resourceId = "AK313848", optionalInfo = ["BAG36577.1","-","mRNA"]},
         DR {resourceAbbr = "EMBL", resourceId = "CH471063", optionalInfo = ["EAW71298.1","-","Genomic_DNA"]},
         DR {resourceAbbr = "EMBL", resourceId = "BC074740", optionalInfo = ["AAH74740.1","-","mRNA"]},
         DR {resourceAbbr = "CCDS", resourceId = "CCDS33428.1", optionalInfo = ["-"]},
         DR {resourceAbbr = "PIR", resourceId = "A55737", optionalInfo = ["A55737"]},
         DR {resourceAbbr = "RefSeq", resourceId = "NP_005009.2", optionalInfo = ["NM_005018.2"]},
         DR {resourceAbbr = "UniGene", resourceId = "Hs.158297", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDB", resourceId = "2M2D", optionalInfo = ["NMR","-","A=34-150"]},
         DR {resourceAbbr = "PDB", resourceId = "3RRQ", optionalInfo = ["X-ray","2.10 A","A=32-160"]},
         DR {resourceAbbr = "PDB", resourceId = "4ZQK", optionalInfo = ["X-ray","2.45 A","B=33-150"]},
         DR {resourceAbbr = "PDB", resourceId = "5B8C", optionalInfo = ["X-ray","2.15 A","C/F/I/L=32-160"]},
         DR {resourceAbbr = "PDB", resourceId = "5GGR", optionalInfo = ["X-ray","3.30 A","Y/Z=26-150"]},
         DR {resourceAbbr = "PDB", resourceId = "5GGS", optionalInfo = ["X-ray","2.00 A","Y/Z=26-148"]},
         DR {resourceAbbr = "PDB", resourceId = "5IUS", optionalInfo = ["X-ray","2.89 A","A/B=26-146"]},
         DR {resourceAbbr = "PDB", resourceId = "5JXE", optionalInfo = ["X-ray","2.90 A","A/B=33-146"]},
         DR {resourceAbbr = "PDB", resourceId = "5WT9", optionalInfo = ["X-ray","2.40 A","G=1-167"]},
         DR {resourceAbbr = "PDBsum", resourceId = "2M2D", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDBsum", resourceId = "3RRQ", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDBsum", resourceId = "4ZQK", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDBsum", resourceId = "5B8C", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDBsum", resourceId = "5GGR", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDBsum", resourceId = "5GGS", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDBsum", resourceId = "5IUS", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDBsum", resourceId = "5JXE", optionalInfo = ["-"]},
         DR {resourceAbbr = "PDBsum", resourceId = "5WT9", optionalInfo = ["-"]},
         DR {resourceAbbr = "ProteinModelPortal", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "SMR", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "BioGrid", resourceId = "111160", optionalInfo = ["61"]},
         DR {resourceAbbr = "DIP", resourceId = "DIP-44126N", optionalInfo = ["-"]},
         DR {resourceAbbr = "IntAct", resourceId = "Q15116", optionalInfo = ["5"]},
         DR {resourceAbbr = "MINT", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "STRING", resourceId = "9606.ENSP00000335062", optionalInfo = ["-"]},
         DR {resourceAbbr = "ChEMBL", resourceId = "CHEMBL3307223", optionalInfo = ["-"]},
         DR {resourceAbbr = "DrugBank", resourceId = "DB05916", optionalInfo = ["CT-011"]},
         DR {resourceAbbr = "DrugBank", resourceId = "DB09035", optionalInfo = ["Nivolumab"]},
         DR {resourceAbbr = "DrugBank", resourceId = "DB09037", optionalInfo = ["Pembrolizumab"]},
         DR {resourceAbbr = "GuidetoPHARMACOLOGY", resourceId = "2760", optionalInfo = ["-"]},
         DR {resourceAbbr = "iPTMnet", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "PhosphoSitePlus", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "BioMuta", resourceId = "PDCD1", optionalInfo = ["-"]},
         DR {resourceAbbr = "DMDM", resourceId = "145559515", optionalInfo = ["-"]},
         DR {resourceAbbr = "PaxDb", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "PeptideAtlas", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "PRIDE", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "DNASU", resourceId = "5133", optionalInfo = ["-"]},
         DR {resourceAbbr = "Ensembl", resourceId = "ENST00000334409", optionalInfo = ["ENSP00000335062","ENSG00000188389"]},
         DR {resourceAbbr = "Ensembl", resourceId = "ENST00000618185", optionalInfo = ["ENSP00000480684","ENSG00000276977"]},
         DR {resourceAbbr = "GeneID", resourceId = "5133", optionalInfo = ["-"]},
         DR {resourceAbbr = "KEGG", resourceId = "hsa:5133", optionalInfo = ["-"]},
         DR {resourceAbbr = "UCSC", resourceId = "uc002wcq.5", optionalInfo = ["human"]},
         DR {resourceAbbr = "CTD", resourceId = "5133", optionalInfo = ["-"]},
         DR {resourceAbbr = "DisGeNET", resourceId = "5133", optionalInfo = ["-"]},
         DR {resourceAbbr = "EuPathDB", resourceId = "HostDB:ENSG00000188389.10", optionalInfo = ["-"]},
         DR {resourceAbbr = "GeneCards", resourceId = "PDCD1", optionalInfo = ["-"]},
         DR {resourceAbbr = "H-InvDB", resourceId = "HIX0030684", optionalInfo = ["-"]},
         DR {resourceAbbr = "HGNC", resourceId = "HGNC:8760", optionalInfo = ["PDCD1"]},
         DR {resourceAbbr = "HPA", resourceId = "CAB038418", optionalInfo = ["-"]},
         DR {resourceAbbr = "HPA", resourceId = "HPA035981", optionalInfo = ["-"]},
         DR {resourceAbbr = "MalaCards", resourceId = "PDCD1", optionalInfo = ["-"]},
         DR {resourceAbbr = "MIM", resourceId = "109100", optionalInfo = ["phenotype"]},
         DR {resourceAbbr = "MIM", resourceId = "600244", optionalInfo = ["gene"]},
         DR {resourceAbbr = "MIM", resourceId = "605218", optionalInfo = ["phenotype"]},
         DR {resourceAbbr = "neXtProt", resourceId = "NX_Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "OpenTargets", resourceId = "ENSG00000188389", optionalInfo = ["-"]},
         DR {resourceAbbr = "Orphanet", resourceId = "802", optionalInfo = ["Multiple sclerosis"]},
         DR {resourceAbbr = "Orphanet", resourceId = "536", optionalInfo = ["Systemic lupus erythematosus"]},
         DR {resourceAbbr = "PharmGKB", resourceId = "PA33110", optionalInfo = ["-"]},
         DR {resourceAbbr = "eggNOG", resourceId = "ENOG410J26W", optionalInfo = ["Eukaryota"]},
         DR {resourceAbbr = "eggNOG", resourceId = "ENOG41116U6", optionalInfo = ["LUCA"]},
         DR {resourceAbbr = "GeneTree", resourceId = "ENSGT00390000013662", optionalInfo = ["-"]},
         DR {resourceAbbr = "HOGENOM", resourceId = "HOG000253959", optionalInfo = ["-"]},
         DR {resourceAbbr = "HOVERGEN", resourceId = "HBG053534", optionalInfo = ["-"]},
         DR {resourceAbbr = "InParanoid", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "KO", resourceId = "K06744", optionalInfo = ["-"]},
         DR {resourceAbbr = "OMA", resourceId = "DFQWREK", optionalInfo = ["-"]},
         DR {resourceAbbr = "OrthoDB", resourceId = "EOG091G0EE8", optionalInfo = ["-"]},
         DR {resourceAbbr = "PhylomeDB", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "TreeFam", resourceId = "TF336181", optionalInfo = ["-"]},
         DR {resourceAbbr = "Reactome", resourceId = "R-HSA-389948", optionalInfo = ["PD-1 signaling"]},
         DR {resourceAbbr = "SIGNOR", resourceId = "Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "ChiTaRS", resourceId = "PDCD1", optionalInfo = ["human"]},
         DR {resourceAbbr = "GeneWiki", resourceId = "Programmed_cell_death_1", optionalInfo = ["-"]},
         DR {resourceAbbr = "GenomeRNAi", resourceId = "5133", optionalInfo = ["-"]},
         DR {resourceAbbr = "PRO", resourceId = "PR:Q15116", optionalInfo = ["-"]},
         DR {resourceAbbr = "Proteomes", resourceId = "UP000005640", optionalInfo = ["Chromosome 2"]},
         DR {resourceAbbr = "Bgee", resourceId = "ENSG00000188389", optionalInfo = ["-"]},
         DR {resourceAbbr = "CleanEx", resourceId = "HS_PDCD1", optionalInfo = ["-"]},
         DR {resourceAbbr = "ExpressionAtlas", resourceId = "Q15116", optionalInfo = ["baseline and differential"]},
         DR {resourceAbbr = "Genevisible", resourceId = "Q15116", optionalInfo = ["HS"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0009897", optionalInfo = ["C:external side of plasma membrane","IEA:Ensembl"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0016021", optionalInfo = ["C:integral component of membrane","IEA:UniProtKB-KW"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0005886", optionalInfo = ["C:plasma membrane","TAS:Reactome"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0004871", optionalInfo = ["F:signal transducer activity","TAS:ProtInc"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0006915", optionalInfo = ["P:apoptotic process","TAS:ProtInc"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0006959", optionalInfo = ["P:humoral immune response","TAS:ProtInc"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0007275", optionalInfo = ["P:multicellular organism development","TAS:ProtInc"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0043066", optionalInfo = ["P:negative regulation of apoptotic process","IEA:Ensembl"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0002644", optionalInfo = ["P:negative regulation of tolerance induction","IEA:Ensembl"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0070234", optionalInfo = ["P:positive regulation of T cell apoptotic process","IDA:UniProtKB"]},
         DR {resourceAbbr = "GO", resourceId = "GO:0031295", optionalInfo = ["P:T cell costimulation","TAS:Reactome"]},
         DR {resourceAbbr = "Gene3D", resourceId = "2.60.40.10", optionalInfo = ["-","1"]},
         DR {resourceAbbr = "InterPro", resourceId = "IPR007110", optionalInfo = ["Ig-like_dom"]},
         DR {resourceAbbr = "InterPro", resourceId = "IPR036179", optionalInfo = ["Ig-like_dom_sf"]},
         DR {resourceAbbr = "InterPro", resourceId = "IPR013783", optionalInfo = ["Ig-like_fold"]},
         DR {resourceAbbr = "InterPro", resourceId = "IPR003599", optionalInfo = ["Ig_sub"]},
         DR {resourceAbbr = "InterPro", resourceId = "IPR013106", optionalInfo = ["Ig_V-set"]},
         DR {resourceAbbr = "Pfam", resourceId = "PF07686", optionalInfo = ["V-set","1"]},
         DR {resourceAbbr = "SMART", resourceId = "SM00409", optionalInfo = ["IG","1"]},
         DR {resourceAbbr = "SMART", resourceId = "SM00406", optionalInfo = ["IGv","1"]},
         DR {resourceAbbr = "SUPFAM", resourceId = "SSF48726", optionalInfo = ["SSF48726","1"]},
         DR {resourceAbbr = "PROSITE", resourceId = "PS50835", optionalInfo = ["IG_LIKE","1"]}]

peAns :: PE
peAns = EvidenceAtProteinLevel

kwAns :: KW
kwAns = KW ["3D-structure", "Apoptosis", "Complete proteome", "Disulfide bond",
            "Glycoprotein", "Immunity", "Immunoglobulin domain", "Membrane", "Polymorphism",
            "Reference proteome", "Signal", "Systemic lupus erythematosus",
            "Transmembrane", "Transmembrane helix"]

ftAns :: [FT]
ftAns = undefined

sqAns :: SQ
sqAns = SQ 288 31647 "A5210FD40C304FB7"
           "MQIPQAPWPVVWAVLQLGWRPGWFLDSPDRPWNPPTFSPALLVVTEGDNATFTCSFSNTSESFVLNWYRMSPSNQTDKLAAFPEDRSQPGQDCRFRVTQLPNGRDFHMSVVRARRNDSGTYLCGAISLAPKAQIKESLRAELRVTERRAEVPTAHPSPSPRPAGQFQTLVVGVVGGLLGSLVLLVWVLAVICSRAARGTIGARRTGQPLKEDPSAVPVFSVDYGELDFQWREKTPEPPVPCVPEQTEYATIVFPSGMGTSSPARRGSADGPRSAQPLRPEDGHCSWPL"

endAns :: ()
endAns = ()

main :: IO ()
main = hspec $ do
    describe "Parse PD1 example" $ do
      it "parses ID lines" $
        parseOnly parseID idStr `shouldBe` Right idAns
      it "parses AC lines" $ 
        parseOnly parseAC acStr `shouldBe` Right acAns
      it "parses DT lines" $
        parseOnly parseDT dtStr `shouldBe` Right dtAns
      it "parses DE lines" $
        parseOnly parseDE deStr `shouldBe` Right deAns
      it "parses GN lines" $
        parseOnly parseGN gnStr `shouldBe` Right gnAns
      it "parses OS lines" $
        parseOnly parseOS osStr `shouldBe` Right osAns
      it "parses OG lines (non-PD1)" $
        parseOnly parseOG ogStr `shouldBe` Right ogAns
      it "parses OC lines" $
        parseOnly parseOC ocStr `shouldBe` Right ocAns
      it "parses OX lines (non-PD1)" $
        parseOnly parseOX oxStr `shouldBe` Right oxAns
      it "parses OH lines (non-PD1)" $
        parseOnly parseOH ohStr `shouldBe` Right ohAns
      it "parses REF lines" $
        parseOnly parseRef refStr `shouldBe` Right refAns
      it "parses CC lines" $
        parseOnly parseCC ccStr `shouldBe` Right ccAns
      it "parses DR lines" $ do
        let parseManyDR = (:) <$> parseDR <*> many' (endOfLine *> parseDR)
        parseOnly parseManyDR drStr `shouldBe` Right drAns
      it "parses PE lines" $
        parseOnly parsePE peStr `shouldBe` Right peAns
      it "parses KW lines" $
        parseOnly parseKW kwStr `shouldBe` Right kwAns
      it "parses FT lines" $
        parseOnly parseFT ftStr `shouldBe` Right ftAns
      it "parses SQ lines" $ do
        parseOnly parseSQ sqStr `shouldBe` Right sqAns
      it "parses End lines" $
        parseOnly parseEnd endStr `shouldBe` Right endAns
      it "parses multiple lines" $ do
        let parser = (,,,,,,,,) <$>          (parseID <* endOfLine)
                                <*>          (parseAC <* endOfLine)
                                <*>          (parseDT <* endOfLine)
                                <*>          (parseDE <* endOfLine)
                                <*>          (parseGN <* endOfLine)
                                <*>          (parseOS <* endOfLine)
                                <*> optional (parseOG <* endOfLine)
                                <*> optional (parseOC <* endOfLine)
                                <*>          (parseSQ <* endOfLine)
                                <*           parseEnd
        let text = unlines $ [idStr, acStr] ++
                             lines dtStr ++
                             lines deStr ++
                             [gnStr] ++
                             lines osStr ++
                             lines ocStr ++
                             lines sqStr ++
                             [endStr]
        parseOnly parser text `shouldBe`
          Right (idAns, acAns, dtAns, deAns,
                 gnAns, osAns, Nothing, Just ocAns, sqAns)
