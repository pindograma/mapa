# match_shared.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

remove_ambiguities = function(x) {
    x %>% group_by(ID) %>%
        filter(n() == 1) %>%
        ungroup()
}

normalize_simple = function(x) {
    stri_trans_general(str = x, id = 'Latin-ASCII') %>%
        str_squish() %>%
        toupper()
}

normalize_place = function(x) {
    normalize_simple(x) %>%
        str_replace_all('[\\.\\-\\,]', ' ') %>%
        str_squish()
}

normalize_school_name = function(x) {
    stage0 = normalize_place(x)

    from = c('E\\s+M\\s+E\\s+I\\s+E\\s+F(\\s+|$)', 'E\\s+M\\s+E\\s+I\\s+F(\\s+|$)',
             'E\\s+M\\s+E\\s+I(\\s+|$)', 'E\\s+M\\s+E\\s+B(\\s+|$)', 'E\\s+M\\s+E\\s+F(\\s+|$)',
             'E\\s+M(\\s+|$)', 'E\\s+E(\\s+|$)', 'C\\s+M\\s+E\\s+I(\\s+|$)')
    to = c('EMEIEF ', 'EMEIF ', 'EMEI ', 'EMEB ', 'EMEF ', 'EM ', 'EE ', 'CMEI ')

    stage1 = mgsub(stage0, from, to)

    from_parana = c('PROFIS(\\s+|$)', 'MOD ED ESP(\\s+|$)',
                    'C E I(\\s+|$)', 'E M EI EF(\\s+|$)',
                    'C E EF M(\\s+|$)', 'E EI EF(\\s+|$)',
                    'E M C EI EF(\\s+|$)', 'E R M EI EF(\\s+|$)',
                    'C E C EF M(\\s+|$)', 'C EI EF M(\\s+|$)',
                    'E R M EF(\\s+|$)', 'E E C EF(\\s+|$)',
                    'E M C EF(\\s+|$)', 'E EF M(\\s+|$)',
                    'E M DO C EF(\\s+|$)', 'E R M DE EF(\\s+|$)',
                    'E M C DE EI EF(\\s+|$)', 'C E EF M N(\\s+|$)',
                    'E E C DE EF(\\s+|$)', 'E E DO C EF(\\s+|$)',
                    'C E EF M N(\\s+|$)')
    to_parana = c(' ', ' ',
                  'CEI ', 'EMEIEF ',
                  'EEFM ', 'EEIEF ',
                  'EMEIEF ', 'EMEIEF ',
                  'EEFM ', 'EEIEFM ',
                  'EMEF ', 'EEF ',
                  'EMEF ', 'EEFM ',
                  'EMEF ', 'EMEF ',
                  'EMEIEF ', 'EEF ',
                  'EEF ', 'EEF ',
                  'EEF ')
    stage2 = mgsub(stage1, from_parana, to_parana)

    from2 = c('GIRASSOL\\s+DE\\s+TEMPO\\s+INTEGRAL(\\s+|$)',
             'EM\\s+TEMPO\\s+INTEGRAL(\\s+|$)',
             'DE\\s+TEMPO\\s+INTEGRAL(\\s+|$)',
             'TEMPO\\s+INTEGRAL(\\s+|$)',
             
             'ESCOLA\\s+DE\\s+REFERENCIA\\s+EM\\s+ENSINO\\s+MEDIO',
             'EE\\s+COLEGIO\\s+ESTADUAL(\\s+|$)',
             'CENTRO\\s+EDUCACIONAL\\s+INFANTIL(\\s+|$)',
             'NUCLEO\\s+DE\\s+EDUCACAO\\s+INFANTIL(\\s+|$)',
             'PRE\\s+ESCOLAR\\s+MUNICIPAL(\\s+|$)',
             'PRE\\s+ESCOLA\\s+MUNICIPAL(\\s+|$)',
             'ESCOLA\\s+BASICA\\s+MUNICIPAL(\\s+|$)',
             'ESCOLA\\s+ISOLADA\\s+MUNICIPAL(\\s+|$)',
             'ESCOLA\\s+PUBLICA\\s+MUNICIPAL(\\s+|$)',
             
             'EDUCACAO\\s+INFANTIL(\\s+|$)',
             'ENSINO\\s+INFANTIL(\\s+|$)',
             'EDUC\\s+INF(\\s+|$)',    
             'ENS\\s+INF(\\s+|$)',          
             'ED\\s+INFANTIL(\\s+|$)',
             
             'EDUCACAO\\s+FUNDAMENTAL(\\s+|$)',
             'EDUCACAO\\s+FUND(\\s+|$)',
             'EDUCACAO\\s+FUN(\\s+|$)',      
             'ENSINO\\s+FUNDAMENTAL(\\s+|$)',
             'ENSINO\\s+FUND(\\s+|$)',
             'ENSINO\\s+FUN(\\s+|$)',
             'ENS\\s+FUNDAMENTAL(\\s+|$)',
             'ENS\\s+FUND(\\s+|$)',
             'ENS\\s+FUN(\\s+|$)',
             
             'ENS\\s+MED(\\s+|$)',
             'ENS\\s+MEDIO(\\s+|$)',
             'ENSINO\\s+MEDIO(\\s+|$)',
             
             'EDUCACAO\\s+BASICA(\\s+|$)',
             'ENSINO\\s+BASICO(\\s+|$)',
             
             'ESCOLA\\s+MUNICIPAL(\\s+|$)',
             'ESCOLA\\s+MUN(\\s+|$)',
             'ESCOLA\\s+MUL(\\s+|$)',
             'ESC\\s+MUNICIPAL(\\s+|$)',
             'ESC\\s+MUN(\\s+|$)',
             'ESC\\s+MUL(\\s+|$)',
             'COLEGIO\\s+MUNICIPAL(\\s+|$)',
             'COL\\s+MUL(\\s+|$)',
             'GRUPO\\s+MUNICIPAL(\\s+|$)',
             'GRUPO\\s+MUL(\\s+|$)',
             'GRUPO\\s+ESCOLAR\\s+MUL(\\s+|$)',
             'GRUPO\\s+ESCOLAR\\s+MUNICIPAL(\\s+|$)',
             'CENTRO\\s+EDUCACIONAL\\s+MUNICIPAL(\\s+|$)',
             'NUCLEO\\s+MUNICIPAL(\\s+|$)',
             'ESCOLA\\s+RURAL\\s+MUNICIPAL(\\s+|$)',
             'UNIDADE\\s+ESCOLAR\\s+MUNICIPAL(\\s+|$)',
             
             'ESCOLA\\s+ESTADUAL(\\s+|$)',
             'ESC\\s+EST(\\s+|$)',
             'COLEGIO\\s+ESTADUAL(\\s+|$)',
             'COL\\s+EST(\\s+|$)',
             'COL\\s+ESTADUAL(\\s+|$)',
             'INST\\s+EST(\\s+|$)',
             'INST\\s+ESTADUAL(\\s+|$)',
             
             'UNIDADE\\s+ESCOLAR(\\s+|$)',
             'UNIDADE\\s+DE\\s+ENSINO(\\s+|$)',
             'UNID\\s+ESC(\\s+|$)',
             'CENTRO\\s+DE\\s+ENSINO(\\s+|$)',
             'ESCOLA\\s+RURAL(\\s+|$)',
             'ESCOLA\\s+ISOLADA(\\s+|$)',
             'GRUPO\\s+ESCOLAR(\\s+|$)',
             'NUCLEO\\s+EDUCACIONAL(\\s+|$)',
             'ESCOLA\\s+PUBLICA(\\s+|$)',
             'PRE\\s+ESCOLAR(\\s+|$)',
             'PRE\\s+ESCOLA(\\s+|$)',
             'PRE\\s+ESC(\\s+|$)',
             'COLEGIO(\\s+|$)',
             'ESCOLA(\\s+|$)',
             'ESC(\\s+|$)',
             'COL(\\s+|$)')
    to2 = c(' ', ' ', ' ', ' ',
           'EEM ', 'EE ', 'CEI ', 'EEI ', 'EM ', 'EM ', 'EMEB ', 'EM ', 'EM ',
           'EI ', 'EI  ', 'EI ', 'EI ', 'EI ',
           'EF ', 'EF ', 'EF ', 'EF ', 'EF ', 'EF ', 'EF ', 'EF ', 'EF ',
           'EM ', 'EM ', 'EM ',
           'EB ', 'EB ',
           'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ', 'EM ',
           'EE ', 'EE ', 'EE ', 'EE ', 'EE ', 'EE ', 'EE ',
           'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ', 'E ')

    stage3 = mgsub(stage2, from2, to2)

    from3 = c(
        'CENTRO\\s+DE\\s+EI(\\s+|$)',
        'CENTRO\\s+DE\\s+EB(\\s+|$)',
        'CENTRO\\s+DE\\s+EF(\\s+|$)',
        
        'CENTRO\\s+MUNICIPAL\\s+DE EB(\\s+|$)',
        'CENTRO\\s+MUNICIPAL\\s+DE EI(\\s+|$)',
        'CENTRO\\s+MUNICIPAL\\s+DE EF(\\s+|$)',
        
        'EM\\s+DE\\s+EB(\\s+|$)',
        'EM\\s+DE\\s+EI(\\s+|$)',
        'EM\\s+DE\\s+EF(\\s+|$)',
        'EM\\s+EB(\\s+|$)',
        'EM\\s+EI(\\s+|$)',
        'EM\\s+EF(\\s+|$)',
        
        'EE\\s+DE\\s+EB(\\s+|$)',
        'EE\\s+DE\\s+EI(\\s+|$)',
        'EE\\s+DE\\s+EF(\\s+|$)',
        'EE\\s+DE\\s+EM(\\s+|$)',
        'EE\\s+EB(\\s+|$)',
        'EE\\s+EI(\\s+|$)',
        'EE\\s+EF(\\s+|$)',
        'EE\\s+EM(\\s+|$)',
        
        'E\\s+DE\\s+EB(\\s+|$)',
        'E\\s+DE\\s+EI(\\s+|$)',
        'E\\s+DE\\s+EF(\\s+|$)',
        'E\\s+DE\\s+EM(\\s+|$)',
        'E\\s+EB(\\s+|$)',
        'E\\s+EI(\\s+|$)',
        'E\\s+EF(\\s+|$)',
        'E\\s+EM(\\s+|$)',
        
        'E\\s+DE\\s+1.?\\s+GRAU(\\s+|$)',
        'EM\\s+DE\\s+1.?\\s+GRAU(\\s+|$)',
        'EE\\s+DE\\s+1.?\\s+GRAU(\\s+|$)',
        'E\\s+DE\\s+PRIMEIRO\\s+GRAU(\\s+|$)',
        'EM\\s+DE\\s+PRIMEIRO\\s+GRAU(\\s+|$)',
        'EE\\s+DE\\s+PRIMEIRO\\s+GRAU(\\s+|$)',
        
        'E\\s+1.?\\s+GRAU(\\s+|$)',
        'EM\\s+1.?\\s+GRAU(\\s+|$)',
        'EE\\s+1.?\\s+GRAU(\\s+|$)',
        'E\\s+PRIMEIRO\\s+GRAU(\\s+|$)',
        'EM\\s+PRIMEIRO\\s+GRAU(\\s+|$)',
        'EE\\s+PRIMEIRO\\s+GRAU(\\s+|$)',
        
        'EEI\\s+E\\s+FUNDAMENTAL(\\s+|$)',
        'EMEI\\s+E\\s+FUNDAMENTAL(\\s+|$)')
    to3 = c('CEI ', 'EEB ', 'EEF ',
            'EMEB ', 'CMEI ', 'EMEF ',
            'EMEB ', 'EMEI ', 'EMEF ', 'EMEB ', 'EMEI ', 'EMEF ',
            'EEEB ', 'EEEI ', 'EEEF ', 'EEEM ', 'EEEB ', 'EEEI ', 'EEEF ', 'EEEM ',
            'EEB ', 'EEI ', 'EEF ', 'EEM ', 'EEB ', 'EEI ', 'EEF ', 'EEM ',
            'EEF ', 'EMEF ', 'EEEF ', 'EEF ', 'EMEF ', 'EEEF ',
            'EEF ', 'EMEF ', 'EEEF ', 'EEF ', 'EMEF ', 'EEEF ',
            'EEIF ', 'EMEIF ')

    stage4 = mgsub(stage3, from3, to3)

    stage5 = mgsub(stage4, from4, to4)
    stage5 = str_squish(stage5)

    stage5 = ifelse(
        grepl('^\\d\\d\\d+', stage5),
        sub('^[^ ]*', '', stage5),
        stage5)

    from5 = c('E INDIGENA ', 'EM INDIGENA ', 'EE INDIGENA ', 'UNIDADE INDIGENA ', 'EMEF INDIGENA ', 'EMEIF INDIGENA ')
    to5 = c('E ', 'EM ', 'EE ', 'E ', 'EMEF ', 'EMEIF ')
    stage6 = mgsub(stage5, from5, to5)

    from6 = c('PREDIO ESCOLAR MUNICIPAL(\\s+|$)', 'PREDIO ESCOLAR(\\s+|$)',
              'CENTRO EDUCACIONAL(\\s+|$)', 'E MUNICIPALIZADA(\\s+|$)',
              'UNIDADE MUNICIPAL DE ENSINO(\\s+|$)', 'ENS.*GRAU$')
    to6 = c('EM ', 'E ', 'E ', 'E ', 'EM ', '')
    stage7 = mgsub(stage6, from6, to6)

    stage8 = move_to_start(stage7, school_words)

    ew_2 = str_trim(to4)
    stage9 = move_to_start_2(stage8, ew_2)

    stage9 = move_to_start(stage9, school_words)

    str_squish(stage9)
}
