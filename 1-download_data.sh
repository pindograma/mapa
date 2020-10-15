#!/bin/bash

pushd data

wget http://pindograma-dados.s3.amazonaws.com/mapa/polling-stations/local-votacao-2008.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/polling-stations/local-votacao-2010.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/polling-stations/local-votacao-2012.xlsx
wget http://pindograma-dados.s3.amazonaws.com/mapa/polling-stations/local-votacao-2014.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/polling-stations/local-votacao-2016.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/polling-stations/local-votacao-08-08-2018.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/polling-stations/local-votacao-2020.csv

mv local-votacao-08-08-2018.csv local-votacao-2018-aug.csv
mv local-votacao-2020.csv local-votacao-2020-jun.csv

# INEP/Locais
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/alagoas.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/estado-saopaulo.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/inep.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/portoalegre.csv
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/recife.geojson

rm -rf cidade-rio-*
rm -rf ibge

mkdir cidade-rio-mun
mkdir cidade-rio-est
mkdir cidade-rio-fed
mkdir ibge

pushd cidade-rio-mun
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-mun/Escolas_Municipais.cpg
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-mun/Escolas_Municipais.dbf
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-mun/Escolas_Municipais.prj
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-mun/Escolas_Municipais.shp
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-mun/Escolas_Municipais.shx
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-mun/Escolas_Municipais.xml
popd

pushd cidade-rio-est
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-est/Escolas_Estaduais.cpg
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-est/Escolas_Estaduais.dbf
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-est/Escolas_Estaduais.prj
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-est/Escolas_Estaduais.shp
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-est/Escolas_Estaduais.shx
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-est/Escolas_Estaduais.xml
popd

pushd cidade-rio-fed
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-fed/Escolas_Federais.cpg
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-fed/Escolas_Federais.dbf
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-fed/Escolas_Federais.prj
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-fed/Escolas_Federais.shp
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-fed/Escolas_Federais.shx
wget http://pindograma-dados.s3.amazonaws.com/mapa/escolas-geocoded/cidade-rio-fed/Escolas_Federais.xml
popd

pushd ibge
wget http://pindograma-dados.s3.amazonaws.com/mapa/ibge-localidades/loc_aglomerado_rural_isolado_p.cpg
wget http://pindograma-dados.s3.amazonaws.com/mapa/ibge-localidades/loc_aglomerado_rural_isolado_p.dbf
wget http://pindograma-dados.s3.amazonaws.com/mapa/ibge-localidades/loc_aglomerado_rural_isolado_p.prj
wget http://pindograma-dados.s3.amazonaws.com/mapa/ibge-localidades/loc_aglomerado_rural_isolado_p.shp
wget http://pindograma-dados.s3.amazonaws.com/mapa/ibge-localidades/loc_aglomerado_rural_isolado_p.shx
popd

# CNEFE Agro 2017
rm -rf cnefe_agro
mkdir cnefe_agro

pushd cnefe_agro

wget -r ftp://ftp.ibge.gov.br/Censo_Agropecuario/Censo_Agropecuario_2017/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos/

for state in $(find . -name *.zip); do
    mv $state .
done

for state in *.zip; do
    unzip $state
    mv $(basename $state .zip)/$(basename $state .zip).csv .
    rm -rf $(basename $state .zip)
    rm $state
done

echo "COD_UF;COD_MUNICIPIO;COD_DISTRITO;COD_SUBDISTRITO;SITUACAO;NOM_TIPO_SEGLOGR;NOM_TITULO_SEGLOGR;NOM_SEGLOGR;NUM_ENDERECO;DSC_MODIFICADOR;NOM_COMP_ELEM1;VAL_COMP_ELEM1;NOM_COMP_ELEM2;VAL_COMP_ELEM2;NOM_COMP_ELEM3;VAL_COMP_ELEM3;NOM_COMP_ELEM4;VAL_COMP_ELEM4;NOM_COMP_ELEM5;VAL_COMP_ELEM5;LATITUDE;LONGITUDE;ALTITUDE;DSC_LOCALIDADE;COD_ESPECIE;CEP" > all.csv2
awk 'FNR > 1' *.csv >> all.csv2
rm *.csv

mv all.csv2 all.csv

popd

# CNEFE 2010
wget http://pindograma-dados.s3.amazonaws.com/cnefe-2010-all.zip
unzip cnefe-2010-all.zip
rm cnefe-2010-all.zip

for city in *.zip; do
    unzip $city
    rm $city
done

for city in *.txt; do
    Rscript ../filter_cnefe.R $city $city.Rdata
    rm $city
done

Rscript ../concatenate_cnefe.R

# Faces e Quadras 2019
rm -rf fq2019
mkdir fq2019

pushd fq2019

wget -r ftp://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2019/

for x in $(find . -name *.zip); do
    mv $x .
done

for x in $(find . -mindepth 1 -type d); do
    rm -rf $x
done

for city in *.zip; do
    unzip $city
    rm $city
done

popd

popd
