export genspio_trap_12_26885=$$ 
 trap 'exit 77' USR1 
 : 
 if {  { { { { true &&  { (  eval "$(printf -- "exec %s>%s" 1 '/tmp/cmd-apt-install-postgresql-client-stdout-0')" || { echo 'Exec "exec %s>%s" 1 '/tmp/cmd-apt-install-postgresql-client-stdout-0' failed' >&2 ; }  
  eval "$(printf -- "exec %s>%s" 2 '/tmp/cmd-apt-install-postgresql-client-stderr-0')" || { echo 'Exec "exec %s>%s" 2 '/tmp/cmd-apt-install-postgresql-client-stderr-0' failed' >&2 ; }  
  {  { 'sudo' 'apt-get' 'update' ; }   ; } 
 ) ; [ $? -eq 0 ] ; } ; } &&  { (  eval "$(printf -- "exec %s>%s" 1 '/tmp/cmd-apt-install-postgresql-client-stdout-1')" || { echo 'Exec "exec %s>%s" 1 '/tmp/cmd-apt-install-postgresql-client-stdout-1' failed' >&2 ; }  
  eval "$(printf -- "exec %s>%s" 2 '/tmp/cmd-apt-install-postgresql-client-stderr-1')" || { echo 'Exec "exec %s>%s" 2 '/tmp/cmd-apt-install-postgresql-client-stderr-1' failed' >&2 ; }  
  {  { 'sudo' 'apt-get' 'upgrade' '--yes' ; }   ; } 
 ) ; [ $? -eq 0 ] ; } ; } &&  { (  eval "$(printf -- "exec %s>%s" 1 '/tmp/cmd-apt-install-postgresql-client-stdout-2')" || { echo 'Exec "exec %s>%s" 1 '/tmp/cmd-apt-install-postgresql-client-stdout-2' failed' >&2 ; }  
  eval "$(printf -- "exec %s>%s" 2 '/tmp/cmd-apt-install-postgresql-client-stderr-2')" || { echo 'Exec "exec %s>%s" 2 '/tmp/cmd-apt-install-postgresql-client-stderr-2' failed' >&2 ; }  
  {  { 'sudo' 'apt-get' 'install' '--yes' 'postgresql-client' ; }   ; } 
 ) ; [ $? -eq 0 ] ; } ; } ; [ $? -eq 0 ] ; } ; } 
 then : 
 else  { 'printf' 'SECOTREC: apt-install-postgresql-client; FAILED:\n\n' ; }  
  { 'printf' '``````````stdout\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-postgresql-client-stdout-0' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stderr\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-postgresql-client-stderr-0' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stdout\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-postgresql-client-stdout-1' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stderr\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-postgresql-client-stderr-1' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stdout\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-postgresql-client-stdout-2' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stderr\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-postgresql-client-stderr-2' ; }  
  { 'printf' '\n``````````\n' ; }  
  { printf -- '%s\n' "EDSL.fail called" >&2 ; kill -s USR1 ${genspio_trap_12_26885} ; }  
 fi 
 if {  { { true &&  { (  eval "$(printf -- "exec %s>%s" 1 '/tmp/cmd-Waiting-for-postgres-stdout-0')" || { echo 'Exec "exec %s>%s" 1 '/tmp/cmd-Waiting-for-postgres-stdout-0' failed' >&2 ; }  
  eval "$(printf -- "exec %s>%s" 2 '/tmp/cmd-Waiting-for-postgres-stderr-0')" || { echo 'Exec "exec %s>%s" 2 '/tmp/cmd-Waiting-for-postgres-stderr-0' failed' >&2 ; }  
  { export $( printf -- "$(printf -- '%s' 103137101124124105115120124123 | sed -e 's/\(.\{3\}\)/\\\1/g')" )="$( printf -- "$(printf -- '%s' "$( { printf -- '%d' 0 ; } | od -t o1 -An -v | tr -d ' \n' )" | sed -e 's/\(.\{3\}\)/\\\1/g')" )" 
 while { { [  $( string_to_int_18_18606=$(  printf -- "$(printf -- '%s' "$( { { getenv_19_33428=$(printf \"\${%s}\" $( printf -- "$(printf -- '%s' 103137101124124105115120124123 | sed -e 's/\(.\{3\}\)/\\\1/g')"  | tr -d '\n')) ; eval "printf -- '%s' "$getenv_19_33428"" ; }  ; } | od -t o1 -An -v | tr -d ' \n' )" | sed -e 's/\(.\{3\}\)/\\\1/g')"  ) ; if [ "$string_to_int_18_18606" -eq "$string_to_int_18_18606" ] ; then printf -- "$string_to_int_18_18606" ; else  { printf -- '%s\n' "String_to_int: error, $string_to_int_18_18606 is not an integer" >&2 ; kill -s USR1 ${genspio_trap_12_26885} ; }  ; fi ; )  -le 40 ] && ! {  {  { 'psql' '-h' 'pg' '-U' 'postgres' '-c' '\l' ; }  ; [ $? -eq 0 ] ; } ; } ; } ; } 
 do  { argument_2_13_95949=$( printf -- "$(printf -- '%s' "$( { printf -- '%d'  $( string_to_int_14_98678=$(  printf -- "$(printf -- '%s' "$( { { getenv_15_3615=$(printf \"\${%s}\" $( printf -- "$(printf -- '%s' 103137101124124105115120124123 | sed -e 's/\(.\{3\}\)/\\\1/g')"  | tr -d '\n')) ; eval "printf -- '%s' "$getenv_15_3615"" ; }  ; } | od -t o1 -An -v | tr -d ' \n' )" | sed -e 's/\(.\{3\}\)/\\\1/g')"  ) ; if [ "$string_to_int_14_98678" -eq "$string_to_int_14_98678" ] ; then printf -- "$string_to_int_14_98678" ; else  { printf -- '%s\n' "String_to_int: error, $string_to_int_14_98678 is not an integer" >&2 ; kill -s USR1 ${genspio_trap_12_26885} ; }  ; fi ; )  ; } | od -t o1 -An -v | tr -d ' \n' )" | sed -e 's/\(.\{3\}\)/\\\1/g')" ; printf 'x') ;  'printf' '%d.' "${argument_2_13_95949%?}" ; }  
  { 'sleep' '2' ; }  
 export $( printf -- "$(printf -- '%s' 103137101124124105115120124123 | sed -e 's/\(.\{3\}\)/\\\1/g')" )="$( printf -- "$(printf -- '%s' "$( { printf -- '%d' $((  $( string_to_int_16_44412=$(  printf -- "$(printf -- '%s' "$( { { getenv_17_62401=$(printf \"\${%s}\" $( printf -- "$(printf -- '%s' 103137101124124105115120124123 | sed -e 's/\(.\{3\}\)/\\\1/g')"  | tr -d '\n')) ; eval "printf -- '%s' "$getenv_17_62401"" ; }  ; } | od -t o1 -An -v | tr -d ' \n' )" | sed -e 's/\(.\{3\}\)/\\\1/g')"  ) ; if [ "$string_to_int_16_44412" -eq "$string_to_int_16_44412" ] ; then printf -- "$string_to_int_16_44412" ; else  { printf -- '%s\n' "String_to_int: error, $string_to_int_16_44412 is not an integer" >&2 ; kill -s USR1 ${genspio_trap_12_26885} ; }  ; fi ; )  + 1 )) ; } | od -t o1 -An -v | tr -d ' \n' )" | sed -e 's/\(.\{3\}\)/\\\1/g')" )" 
 done 
  { 'printf' '\n' ; }  
 if { [  $( string_to_int_20_92869=$(  printf -- "$(printf -- '%s' "$( { { getenv_21_7289=$(printf \"\${%s}\" $( printf -- "$(printf -- '%s' 103137101124124105115120124123 | sed -e 's/\(.\{3\}\)/\\\1/g')"  | tr -d '\n')) ; eval "printf -- '%s' "$getenv_21_7289"" ; }  ; } | od -t o1 -An -v | tr -d ' \n' )" | sed -e 's/\(.\{3\}\)/\\\1/g')"  ) ; if [ "$string_to_int_20_92869" -eq "$string_to_int_20_92869" ] ; then printf -- "$string_to_int_20_92869" ; else  { printf -- '%s\n' "String_to_int: error, $string_to_int_20_92869 is not an integer" >&2 ; kill -s USR1 ${genspio_trap_12_26885} ; }  ; fi ; )  -gt 40 ] ; } 
 then  { 'printf' 'SECOTREC: Command failed 40 times!\n' ; }  
  { 'sh' '-c' 'exit 2' ; }  
 else : 
 fi  ; } 
 ) ; [ $? -eq 0 ] ; } ; } ; [ $? -eq 0 ] ; } ; } 
 then : 
 else  { 'printf' 'SECOTREC: Waiting-for-postgres; FAILED:\n\n' ; }  
  { 'printf' '``````````stdout\n' ; }  
  { 'cat' '/tmp/cmd-Waiting-for-postgres-stdout-0' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stderr\n' ; }  
  { 'cat' '/tmp/cmd-Waiting-for-postgres-stderr-0' ; }  
  { 'printf' '\n``````````\n' ; }  
  { printf -- '%s\n' "EDSL.fail called" >&2 ; kill -s USR1 ${genspio_trap_12_26885} ; }  
 fi 
 if {  { { { { true &&  { (  eval "$(printf -- "exec %s>%s" 1 '/tmp/cmd-apt-install-docker_io-stdout-0')" || { echo 'Exec "exec %s>%s" 1 '/tmp/cmd-apt-install-docker_io-stdout-0' failed' >&2 ; }  
  eval "$(printf -- "exec %s>%s" 2 '/tmp/cmd-apt-install-docker_io-stderr-0')" || { echo 'Exec "exec %s>%s" 2 '/tmp/cmd-apt-install-docker_io-stderr-0' failed' >&2 ; }  
  {  { 'sudo' 'apt-get' 'update' ; }   ; } 
 ) ; [ $? -eq 0 ] ; } ; } &&  { (  eval "$(printf -- "exec %s>%s" 1 '/tmp/cmd-apt-install-docker_io-stdout-1')" || { echo 'Exec "exec %s>%s" 1 '/tmp/cmd-apt-install-docker_io-stdout-1' failed' >&2 ; }  
  eval "$(printf -- "exec %s>%s" 2 '/tmp/cmd-apt-install-docker_io-stderr-1')" || { echo 'Exec "exec %s>%s" 2 '/tmp/cmd-apt-install-docker_io-stderr-1' failed' >&2 ; }  
  {  { 'sudo' 'apt-get' 'upgrade' '--yes' ; }   ; } 
 ) ; [ $? -eq 0 ] ; } ; } &&  { (  eval "$(printf -- "exec %s>%s" 1 '/tmp/cmd-apt-install-docker_io-stdout-2')" || { echo 'Exec "exec %s>%s" 1 '/tmp/cmd-apt-install-docker_io-stdout-2' failed' >&2 ; }  
  eval "$(printf -- "exec %s>%s" 2 '/tmp/cmd-apt-install-docker_io-stderr-2')" || { echo 'Exec "exec %s>%s" 2 '/tmp/cmd-apt-install-docker_io-stderr-2' failed' >&2 ; }  
  {  { 'sudo' 'apt-get' 'install' '--yes' 'docker.io' ; }   ; } 
 ) ; [ $? -eq 0 ] ; } ; } ; [ $? -eq 0 ] ; } ; } 
 then : 
 else  { 'printf' 'SECOTREC: apt-install-docker.io; FAILED:\n\n' ; }  
  { 'printf' '``````````stdout\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-docker_io-stdout-0' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stderr\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-docker_io-stderr-0' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stdout\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-docker_io-stdout-1' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stderr\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-docker_io-stderr-1' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stdout\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-docker_io-stdout-2' ; }  
  { 'printf' '\n``````````\n' ; }  
  { 'printf' '``````````stderr\n' ; }  
  { 'cat' '/tmp/cmd-apt-install-docker_io-stderr-2' ; }  
  { 'printf' '\n``````````\n' ; }  
  { printf -- '%s\n' "EDSL.fail called" >&2 ; kill -s USR1 ${genspio_trap_12_26885} ; }  
 fi 
  { 'sudo' 'usermod' '-aG' 'docker' 'opam' ; }  
  { 'sudo' 'chmod' '666' '/var/run/docker.sock' ; }  
 if {  {  { 'coclobas' 'config' '--root' '/tmp/cocloroot' '--database-uri' 'postgresql://pg/?user=postgres&password=kpass' '--cluster-kind' 'local-docker' '--max-nodes' '2' ; }  ; [ $? -eq 0 ] ; } ; } 
 then  { 'coclobas' 'start-server' '--root' '/tmp/cocloroot' '--port' '8082' ; }  
 else  { 'echo' 'coclobas config failed' ; }  
 fi