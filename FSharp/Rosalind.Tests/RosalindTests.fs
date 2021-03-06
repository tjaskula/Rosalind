﻿namespace Rosalind.Tests

open Xunit
open FsCheck
open Rosalind

module ``Bioinformatics Stronghlod tests`` =
 
    // DNA : Counting DNA Nucleotides
    let rawDna = "CCCGCCTTCTCTTCGCTTGAGAGTGCGTCGGTTCGGCGGCATTTACGGAGCCACAGCCACAACCC\
                          ACCCTACATCGTAGGTAGTTTAAGGCCAGTCCCTAATGCGTTCGGGGTAGAGTAAGCGAGTCCCT\
                          TCGAGGACCCAAGCTTTTTTCGATGAAACGCGCTTGTATTCTAGTGCTCGCCCGACTACAAGGAG\
                          CCATCGACGTAGGCCGTTAACAAACCTAGATTCTCAGCTGTTTCGTGGAGGACCGCGCAGAAGCT\
                          CCGGATACGCGTTTCGACACGGTAGCCCCCCCGTCTGTGATAGCAGCGAAGAGATTCTTCACAAC\
                          GGTGCGGACTCGAGGGATTCCCTAGTAAGCCTTCGCCCGATCGGGCTACTTACATTCCCTCGCGC\
                          TACTCTCGTAGGGCCAGGTAGGCCAACGCGTTCCGGTACCCGTTTAGTTCTTGTGGGGATCATGG\
                          GAGAGGTATAGTTGCTCGGGCGGGGTGAGCATCGCAGACTCGTTTTTCACTGCGCTCGCGCATCT\
                          GCCACGAGGACATTCGATCATGTGGCGACGTGATCCCTCTTCAGACCTTATCTTGGAGGGCGCAG\
                          CCGTCCGTTCCACCCGAATGTCCGCGGTTTTGGCGACACTGGTAACCTTAGCAGTCAGTACGCGT\
                          AATAGCGACGGGCCCAAGGCTGCGCGTCCACTCGATTGTAGTGCTTCAGGACCTAGCGCTGCTGA\
                          GATAATGGATACAGTAGGCGAAATCGTGATAGTTGCTATGCCGGCCGTAATCAGAACAAACTAAT\
                          GTCGCCAATACTCCACGGCGCACCTATTGACCACGTTTCTTTCCCCATCCCTTTAAATTCCCGCT\
                          GTCCCGGGCATCGGAATAGGAAGGGACTGAGTACTAGAGAGTCCTGCTGGGTC"
 
    [<Fact>]
    let ``DNA : Counting DNA Nucleotides``() =
        let result = dna rawDna = (185, 256, 245, 212)
        Check.Quick result

    [<Fact>]
    let ``DNA' : Counting DNA Nucleotides``() =
        let result = dna' rawDna = [185; 256; 245; 212]
        Check.Quick result

    [<Fact>]
    let ``DNA'' : Counting DNA Nucleotides``() =
        let result = dna'' rawDna = [('C', 256); ('G', 245); ('T', 212); ('A', 185)]
        Check.Quick result

    let dna = "CTATGGGCTCGTGTCCTCCGATTTGCGTCACCTGGATCATGCGGATGTATATTAAACCTTTGTATCTTGTGGAGGG\
               AATGGATCTAGGGCAGCCGAGTGGCCCCAGTGTATTATCTAAATGAGCGTGCCTAGCTCCTCTTTCTCGCACCGCA\
               TCCTTTGGAATTTAAGGAATGCGGCAAGAAGACAAACCGTCCCCCTTGGTCCTACACCACGACTGGGAGATGTGTG\
               GTTGATGGTGGTACGAGGCTAAGGTAGACGCTATATGGCAAATTTTGCCACTATACGCATTGTTTATTAGGTTTCG\
               TGCGCCTGCCCACCCTGAGACCCCGCAGCACACACCTAGGGTGCATTGTCATCACGACCAAGATTCAACAGACTGA\
               GTAAGGATAGAGCTGGATATGTTTTTGAGACTCTCTCAACATCGCGGTCAGGTCTACGAGTGAGTTTTGCCCGCCT\
               AATCCTCTTGTTAAACAGCCACCTCGCTGGTAACTCGGAATTAAGAGGGGCAACAGAATCCACCGATTCCCTAACA\
               AATGGAGCTCCTTACGAATAAGCGCAGGCGGCAACCATAGCTTAATGCCCGCCGGTCTTACCTTCCTTTAGATGTT\
               CAGGCCACAAATAGGAATGAAACTCTACCACCGTCACAGAGAACTTAAAACGGCGAATGAGGCAATTTATACACGA\
               CCTCGATGGCGTAACAGCTTCAGGCTGAGTGCTTCCGGGCATCAGTTAGACTGTTGCTAACTAGTCTCAGCGTCGG\
               TTCGGCCTCGACTGGGAGGCTACTATTCTGCAGGTACGTTACTGGCTCATGTGAATTGGCAAGAATCCCTCGAGGC\
               AGCTTTTTCTCACTGAGCTATTTGCCTTGTCAAATACCCCGTACGGTGAGTCGGTACGCAGGTAGGACATACTGCG\
               ATTACGGGAATAATTTTAAGCTAGCGGGTAACTTGTACGCTAAGTCGTTAAGCGTCTGTACCG";

    [<Fact>]
    let ``RNA : Transcribing DNA into RNA``() =
        let result = transformToRna dna = "CUAUGGGCUCGUGUCCUCCGAUUUGCGUCACCUGGAUCAUGCGGAUGUAUAUUAAACCUUUGUAUCUUGUGGAGGGAA\
                                           UGGAUCUAGGGCAGCCGAGUGGCCCCAGUGUAUUAUCUAAAUGAGCGUGCCUAGCUCCUCUUUCUCGCACCGCAUCCU\
                                           UUGGAAUUUAAGGAAUGCGGCAAGAAGACAAACCGUCCCCCUUGGUCCUACACCACGACUGGGAGAUGUGUGGUUGAU\
                                           GGUGGUACGAGGCUAAGGUAGACGCUAUAUGGCAAAUUUUGCCACUAUACGCAUUGUUUAUUAGGUUUCGUGCGCCUG\
                                           CCCACCCUGAGACCCCGCAGCACACACCUAGGGUGCAUUGUCAUCACGACCAAGAUUCAACAGACUGAGUAAGGAUAG\
                                           AGCUGGAUAUGUUUUUGAGACUCUCUCAACAUCGCGGUCAGGUCUACGAGUGAGUUUUGCCCGCCUAAUCCUCUUGUU\
                                           AAACAGCCACCUCGCUGGUAACUCGGAAUUAAGAGGGGCAACAGAAUCCACCGAUUCCCUAACAAAUGGAGCUCCUUA\
                                           CGAAUAAGCGCAGGCGGCAACCAUAGCUUAAUGCCCGCCGGUCUUACCUUCCUUUAGAUGUUCAGGCCACAAAUAGGA\
                                           AUGAAACUCUACCACCGUCACAGAGAACUUAAAACGGCGAAUGAGGCAAUUUAUACACGACCUCGAUGGCGUAACAGC\
                                           UUCAGGCUGAGUGCUUCCGGGCAUCAGUUAGACUGUUGCUAACUAGUCUCAGCGUCGGUUCGGCCUCGACUGGGAGGC\
                                           UACUAUUCUGCAGGUACGUUACUGGCUCAUGUGAAUUGGCAAGAAUCCCUCGAGGCAGCUUUUUCUCACUGAGCUAUU\
                                           UGCCUUGUCAAAUACCCCGUACGGUGAGUCGGUACGCAGGUAGGACAUACUGCGAUUACGGGAAUAAUUUUAAGCUAG\
                                           CGGGUAACUUGUACGCUAAGUCGUUAAGCGUCUGUACCG"
        Check.Quick result

    [<Fact>]
    let ``REVC : Complementing a Strand of DNA``() =
        let result = revComplement (readFromFile "../../../Data/rosalind_revc.txt") = readFromFile "../../../Data/rosalind_revc_output.txt"
        Check.Quick result

module ``Bioinformatics Armory tests`` =

    // INI : Introduction to the Bioinformatics Armory
    let rawDna = "CCCGCCTTCTCTTCGCTTGAGAGTGCGTCGGTTCGGCGGCATTTACGGAGCCACAGCCACAACCC\
                          ACCCTACATCGTAGGTAGTTTAAGGCCAGTCCCTAATGCGTTCGGGGTAGAGTAAGCGAGTCCCT\
                          TCGAGGACCCAAGCTTTTTTCGATGAAACGCGCTTGTATTCTAGTGCTCGCCCGACTACAAGGAG\
                          CCATCGACGTAGGCCGTTAACAAACCTAGATTCTCAGCTGTTTCGTGGAGGACCGCGCAGAAGCT\
                          CCGGATACGCGTTTCGACACGGTAGCCCCCCCGTCTGTGATAGCAGCGAAGAGATTCTTCACAAC\
                          GGTGCGGACTCGAGGGATTCCCTAGTAAGCCTTCGCCCGATCGGGCTACTTACATTCCCTCGCGC\
                          TACTCTCGTAGGGCCAGGTAGGCCAACGCGTTCCGGTACCCGTTTAGTTCTTGTGGGGATCATGG\
                          GAGAGGTATAGTTGCTCGGGCGGGGTGAGCATCGCAGACTCGTTTTTCACTGCGCTCGCGCATCT\
                          GCCACGAGGACATTCGATCATGTGGCGACGTGATCCCTCTTCAGACCTTATCTTGGAGGGCGCAG\
                          CCGTCCGTTCCACCCGAATGTCCGCGGTTTTGGCGACACTGGTAACCTTAGCAGTCAGTACGCGT\
                          AATAGCGACGGGCCCAAGGCTGCGCGTCCACTCGATTGTAGTGCTTCAGGACCTAGCGCTGCTGA\
                          GATAATGGATACAGTAGGCGAAATCGTGATAGTTGCTATGCCGGCCGTAATCAGAACAAACTAAT\
                          GTCGCCAATACTCCACGGCGCACCTATTGACCACGTTTCTTTCCCCATCCCTTTAAATTCCCGCT\
                          GTCCCGGGCATCGGAATAGGAAGGGACTGAGTACTAGAGAGTCCTGCTGGGTC"

    [<Fact>]
    let ``INI : Introduction to the Bioinformatics Armory``() =
        let result = ini rawDna = "185 256 245 212"
        Check.Quick result

    [<Fact>]
    let ``DBPR : Introduction to Protein Databases``() =
        let result = findMatches "../../../Data/proteins.txt" "DR\s*GO;\s*GO:\d*;\sP:([\w\s]*)" = "GTP catabolic process\nintracellular protein transport\nnucleocytoplasmic transport\nsmall GTPase mediated signal transduction"
        Check.Quick result

    [<Fact>]
    let ``GBK : GenBank Introduction``() =
        let result = countBase() = 22
        Check.Quick result

module ``Bioinformatics Textbook Track tests`` =

    let data = "TCTTCCACATCTAGCGGAACCTTAGTCTTCCACACCCTTAAGCCCTTAAGCCCTTAAGTCTAGCGGACCTCATGGACCTCATGGTCTAGCGGTC\
                TAGCGGACCTCATGGCCCTTAAGACCTCATGGCCCTTAAGACCTCATGGTCTTCCACATCTTCCACAACCTCATGGTCTAGCGGAACCTTAGTC\
                TAGCGGTCTAGCGGTCTTCCACATCTAGCGGCCCTTAAGCCCTTAAGACCTCATGGAACCTTAGAACCTTAGACCTCATGGTCTTCCACAACCT\
                CATGGTCTTCCACATCTAGCGGTCTAGCGGTCTAGCGGACCTCATGGTCTTCCACATCTTCCACATCTAGCGGACCTCATGGCCCTTAAGACCT\
                CATGGACCTCATGGCCCTTAAGTCTAGCGGTCTTCCACAAACCTTAGTCTAGCGGAACCTTAGTCTTCCACATCTAGCGGTCTTCCACAACCTC\
                ATGGAACCTTAGTCTAGCGGCCCTTAAGACCTCATGGTCTTCCACATCTAGCGGTCTTCCACATCTTCCACACCCTTAAGTCTTCCACATCTAG\
                CGGTCTTCCACAACCTCATGGTCTAGCGGTCTAGCGGAACCTTAGTCTAGCGGACCTCATGGACCTCATGGAACCTTAGACCTCATGGAACCTT\
                AGCCCTTAAGTCTAGCGGTCTAGCGGCCCTTAAGAACCTTAGACCTCATGGTCTTCCACAACCTCATGGTCTAGCGGCCCTTAAGTCTAGCGGT\
                CTTCCACAAACCTTAGCCCTTAAGCCCTTAAGTCTTCCACACCCTTAAGACCTCATGGACCTCATGGAACCTTAGAACCTTAGAACCTTAGTCTAGCGGACCTCATGG"

    [<Fact>]
    let ``1A : Frequent Words Problem``() =
        let result = mostFrequent 13 data = "GTCTTCCACATCT"
        Check.Quick result

    [<Fact>]
    let ``1B : Reverse Complement Problem``() =
        let result = revComplement (readFromFile "../../../Data/reverse.txt") = readFromFile "../../../Data/reverse_output.txt"
        Check.Quick result

    [<Fact>]
    let ``1C : Pattern Matching Problem``() =
        let result = patternMatch() = readFromFile "../../../Data/rosalind_1c_output.txt"
        Check.Quick result

module ``Algorithmic Heights tests`` =
    
    [<Fact>]
    let ``FIBO : Fibonacci Numbers``() =
        let result = fibo 20 = 6765
        Check.Quick result

    [<Fact>]
    let ``BINS : Binary Search``() =
        let result = doSearch "../../../Data/rosalind_bins.txt" = readFromFile "../../../Data/rosalind_bins_output.txt"
        Check.Quick result