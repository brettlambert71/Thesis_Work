#!/bin/bash

chmod +x *.sh
source ~/miniconda3/bin/activate
export PATH=$PATH:/users/PAS1596/lambert648/miniconda3
conda activate ISSR_Seq_Analysis
export PATH=$PATH:/users/PAS1596/lambert648/miniconda3/pkgs/picard-3.3.0-hdfd78af_0/share/picard-3.3.0-0
export PATH=$PATH:/fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/gatk-4.6.1.0/
export PATH=$PATH:/fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/samtools-1.21/bin
export PATH=$PATH:/fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/samtools-1.21
export PATH=$PATH:/fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/jdk-17.0.12/bin
module load vcftools
export PATH=$PATH:/fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/gatk-4.6.1.0/
export PATH=$PATH:/fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/vcf2phylip
export PATH=$PATH:/fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/vcftools_0.1.7
export PATH=/fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/FullOut_2025_01_21_09_31gatk --java-options "-Xmx100g" \
SelectVariants \
-R /fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/FullOut_2025_01_21_09_31/reference/final_reference_assembly.fa \
-V /fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/FullOut_2025_01_21_09_31/variants/raw_variants.vcf \
-O /fs/ess/PAS0331/Brett/Analysis_Software/ISSRseq/FullOut_2025_01_21_09_31/variants/filtered_variants.vcf \
--select-type-to-include SNP \
--select-type-to-include INDEL \
--select 'QD > 2.0 && FS < 60.0 && SOR < 3.0 && ReadPosRankSum > -8.0 && MQRankSum > -12.5 && QUAL > 30.0' \
