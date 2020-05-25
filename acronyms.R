# acronyms.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

from4 = c('DOUTOR(\\s+|$)', 'DOUTORA(\\s+|$)',
      'PROFESSOR(\\s+|$)', 'PROFESSORA(\\s+|$)', 'PROFA(\\s+|$)', 'PROFª(\\s+|$)', 'PROFº(\\s+|$)',
      'PADRE(\\s+|$)',
      'MAJOR(\\s+|$)', 'MINISTRO(\\s+|$)', 'MARECHAL(\\s+|$)', 'SENADOR(\\s+|$)', 'CORONEL(\\s+|$)', 'CAPITAO(\\s+|$)',
      'SANTA(\\s+|$)', 'SANTO(\\s+|$)', 'SAO(\\s+|$)', 'PRESIDENTE(\\s+|$)', 'GENERAL(\\s+|$)', 'DEPUTADO(\\s+|$)', 'DEPUTADA(\\s+|$)',
      'PREFEITO(\\s+|$)', 'VEREADOR(\\s+|$)', 'ENGENHEIRO(\\s+|$)', 'FREI(\\s+|$)', 'DONA(\\s+|$)', 'COMENDADOR(\\s+|$)',
      'GOVERNADOR(\\s+|$)', 'DOM(\\s+|$)', 'BARAO(\\s+|$)', 'DUQUE(\\s+|$)',
      'TENENTE(\\s|$)', 'VISCONDE(\\s|$)', 'DESEMBARGADOR(\\s|$)',
      'N\\s+S(\\s|$)', 'NOSSA\\s+SENHORA(\\s|$)')
to4 = c('DR ', 'DRA ',
        'PROF ', 'PROF ', 'PROF ', 'PROF ', 'PROF ',
        'PE ',
        'MAJ ', 'MIN ', 'MAL ', 'SEN ', 'CEL ', 'CAP ',
        'S ', 'S ', 'S ', 'PRES ', 'GAL ', 'DEP ', 'DEP ',
        'PREF ', 'VER ', 'ENG ', 'FR ', 'DA ', 'COM ',
        'GOV ', 'D ', 'BR ', 'DQ ',
        'TEN ', 'VISC ', 'DES ',
        'NS ', 'NS ')

school_words = c( 'E', 'EM', 'EMEB', 'EMEF', 'EMEI', 'CEI', 'CMEI', 'CEMEI',
                 'EMEIEF', 'EMEIF',
                 'EE', 'EEEB', 'EEEF', 'EEEI', 'EEEM', 'ETEC', 'EEFM',
                 'EEB', 'EEF', 'EEI', 'EEM', 'EEIEF', 'EEIF', 'EEF', 'EPG', 'EP',
                 'CEM', 'CEMEB', 'EMEFM')
