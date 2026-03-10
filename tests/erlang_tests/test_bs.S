{module, test_bs}.  %% version = 0

{exports, [{ext_id,1},
           {join,2},
           {module_info,0},
           {module_info,1},
           {start,0},
           {start_1,0},
           {start_2,0},
           {start_3,0},
           {start_3a,0},
           {start_3a1,0},
           {start_3a2,0},
           {start_3a3,0},
           {start_3a3_a,0},
           {start_3a3_b,0},
           {start_3a3_c,0},
           {start_3a3_d,0},
           {start_3a3_e,0},
           {start_3a4,0},
           {start_3a5,0},
           {start_3b,0},
           {start_4,0}]}.

{attributes, []}.

{labels, 459}.


{function, start, 0, 2}.
  {label,1}.
    {line,[{location,"test_bs.erl",28}]}.
    {func_info,{atom,test_bs},{atom,start},0}.
  {label,2}.
    {allocate,0,0}.
    {line,[{location,"test_bs.erl",29}]}.
    {call,0,{f,4}}. % start_1/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {line,[{location,"test_bs.erl",30}]}.
    {call,0,{f,9}}. % start_2/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {line,[{location,"test_bs.erl",31}]}.
    {call,0,{f,29}}. % start_3/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {call_last,0,{f,60},0}. % start_4/0


{function, start_1, 0, 4}.
  {label,3}.
    {line,[{location,"test_bs.erl",36}]}.
    {func_info,{atom,test_bs},{atom,start_1},0}.
  {label,4}.
    {allocate,0,0}.
    {move,{literal,<<23,180>>},{x,1}}.
    {move,{literal,{2,61,20}},{x,0}}.
    {line,[{location,"test_bs.erl",37}]}.
    {call,2,{f,62}}. % test_pack_small_ints/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,16}}]}}.
    {move,{integer,32},{x,1}}.
    {move,{literal,<<0,0,4,0>>},{x,2}}.
    {move,{integer,1024},{x,0}}.
    {line,[{location,"test_bs.erl",38}]}.
    {call,3,{f,65}}. % test_pack_integer_big_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,32}}]}}.
    {move,{literal,<<"fubar">>},{x,2}}.
    {move,{literal,<<"Haddock's Eyes">>},{x,3}}.
    {move,{integer,2},{x,1}}.
    {move,{literal,<<0,0,0,15,0,2,102,117,98,97,114,72,97,100>>},{x,4}}.
    {move,{integer,15},{x,0}}.
    {line,[{location,"test_bs.erl",39}]}.
    {call,5,{f,68}}. % test_pack_integers_and_binaries/5
    {'%',{var_info,{x,0},[{type,{t_bitstring,112}}]}}.
    {move,{literal,<<"fubar">>},{x,3}}.
    {move,{integer,2},{x,2}}.
    {move,{literal,<<"Had">>},{x,4}}.
    {move,{integer,15},{x,1}}.
    {line,[{location,"test_bs.erl",46}]}.
    {call,5,{f,71}}. % test_unpack_integers_and_binaries/5
    {'%',{var_info,{x,0},[{type,{t_bitstring,112}}]}}.
    {line,[{location,"test_bs.erl",48}]}.
    {call,0,{f,78}}. % test_create_with_invalid_int_value/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",49}]}.
    {call,0,{f,80}}. % test_create_with_invalid_int_size/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",50}]}.
    {call,0,{f,82}}. % test_create_with_int_unit/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",51}]}.
    {call,0,{f,88}}. % test_create_with_unsupported_unaligned_int_size/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",52}]}.
    {call,0,{f,90}}. % test_create_with_int_little_endian/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",53}]}.
    {call,0,{f,109}}. % test_create_with_int_signed/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",54}]}.
    {call,0,{f,111}}. % test_create_with_invalid_binary_value/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",55}]}.
    {call,0,{f,113}}. % test_create_with_invalid_binary_size/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",56}]}.
    {call,0,{f,115}}. % test_create_with_binary_size_out_of_range/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",57}]}.
    {call,0,{f,117}}. % test_create_with_unsupported_binary_unit/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {move,{integer,8},{x,1}}.
    {move,{literal,<<15>>},{x,0}}.
    {line,[{location,"test_bs.erl",59}]}.
    {call,2,{f,189}}. % get_integer_big_unsigned/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,7},[{tr,{x,0},{t_integer,any}},{integer,15}]}.
    {move,{integer,8},{x,1}}.
    {move,{literal,<<128>>},{x,0}}.
    {line,[{location,"test_bs.erl",60}]}.
    {call,2,{f,189}}. % get_integer_big_unsigned/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,6},[{tr,{x,0},{t_integer,any}},{integer,128}]}.
    {move,{integer,32},{x,1}}.
    {move,{literal,<<0,0,17,52>>},{x,0}}.
    {line,[{location,"test_bs.erl",61}]}.
    {call,2,{f,189}}. % get_integer_big_unsigned/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,5},[{tr,{x,0},{t_integer,any}},{integer,4404}]}.
    {line,[{location,"test_bs.erl",63}]}.
    {call,0,{f,119}}. % test_get_with_unsupported_int_unit/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",64}]}.
    {call,0,{f,121}}. % test_get_with_int_little_endian/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",65}]}.
    {call,0,{f,143}}. % test_get_with_int_signed/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",66}]}.
    {call,0,{f,173}}. % test_get_with_unaligned_binary/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.
  {label,5}.
    {line,[{location,"test_bs.erl",61}]}.
    {badmatch,{x,0}}.
  {label,6}.
    {line,[{location,"test_bs.erl",60}]}.
    {badmatch,{x,0}}.
  {label,7}.
    {line,[{location,"test_bs.erl",59}]}.
    {badmatch,{x,0}}.


{function, start_2, 0, 9}.
  {label,8}.
    {line,[{location,"test_bs.erl",71}]}.
    {func_info,{atom,test_bs},{atom,start_2},0}.
  {label,9}.
    {allocate,1,0}.
    {init_yregs,{list,[{y,0}]}}.
    {move,{literal,<<"ÿ">>},{x,0}}.
    {line,[{location,"test_bs.erl",72}]}.
    {call,1,{f,237}}. % test_match_first_integer/1
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[nope]},
                                   none,none,none,
                                   {t_bitstring,8}}}]}}.
    {test,is_eq_exact,
          {f,27},
          [{tr,{x,0},{t_union,{t_atom,[nope]},none,none,none,{t_bitstring,8}}},
           {literal,<<>>}]}.
    {move,{literal,<<255,1,2,3>>},{x,0}}.
    {line,[{location,"test_bs.erl",73}]}.
    {call,1,{f,237}}. % test_match_first_integer/1
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[nope]},
                                   none,none,none,
                                   {t_bitstring,8}}}]}}.
    {test,is_eq_exact,
          {f,26},
          [{tr,{x,0},{t_union,{t_atom,[nope]},none,none,none,{t_bitstring,8}}},
           {literal,<<1,2,3>>}]}.
    {move,{literal,<<171,205,1,2,3>>},{x,0}}.
    {line,[{location,"test_bs.erl",74}]}.
    {call,1,{f,237}}. % test_match_first_integer/1
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[nope]},
                                   none,none,none,
                                   {t_bitstring,8}}}]}}.
    {test,is_eq_exact,
          {f,25},
          [{tr,{x,0},{t_union,{t_atom,[nope]},none,none,none,{t_bitstring,8}}},
           {literal,<<1,2,3>>}]}.
    {move,{literal,<<0,1,2,3>>},{x,0}}.
    {line,[{location,"test_bs.erl",75}]}.
    {call,1,{f,237}}. % test_match_first_integer/1
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[nope]},
                                   none,none,none,
                                   {t_bitstring,8}}}]}}.
    {test,is_eq_exact,
          {f,24},
          [{tr,{x,0},{t_union,{t_atom,[nope]},none,none,none,{t_bitstring,8}}},
           {atom,nope}]}.
    {move,{literal,<<1,2,3,4,5,6>>},{x,1}}.
    {move,{literal,<<1,2,3>>},{x,0}}.
    {line,[{location,"test_bs.erl",77}]}.
    {call,2,{f,242}}. % test_bs_append/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,23},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<1,2,3,1,2,3,4,5,6>>}]}.
    {move,{literal,<<1,2,3>>},{x,0}}.
    {line,[{location,"test_bs.erl",79}]}.
    {call,1,{f,244}}. % test_bs_private_append/1
    {test,is_eq_exact,{f,22},[{x,0},{literal,<<1,2,3>>}]}.
    {move,{integer,0},{x,1}}.
    {move,{literal,<<>>},{x,2}}.
    {move,{integer,32},{x,0}}.
    {line,[{location,"test_bs.erl",82}]}.
    {call,3,{f,270}}. % make_binary_copy/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {move,{x,0},{y,0}}.
    {move,{integer,240},{x,1}}.
    {move,{literal,<<>>},{x,2}}.
    {move,{integer,32},{x,0}}.
    {line,[{location,"test_bs.erl",83}]}.
    {call,3,{f,270}}. % make_binary_copy/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {call,1,{f,398}}. % id/1
    {move,{literal,<<>>},{x,1}}.
    {call,2,{f,246}}. % test_bs_private_append2/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,21},
          [{tr,{x,0},{t_bitstring,1}},{tr,{y,0},{t_bitstring,1}}]}.
    {trim,1,0}.
    {move,{literal,<<>>},{x,0}}.
    {line,[{location,"test_bs.erl",85}]}.
    {call,1,{f,251}}. % test_match_clause/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     none,none,
                     [{{3,{t_integer,{99,99}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{99,99}},
                             2 => {t_integer,{0,4294967295}},
                             3 => {t_bitstring,8}}}},
                      {{3,{t_integer,{110,110}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{110,110}},
                             2 => {t_bitstring,8},
                             3 => {t_bitstring,8}}}}],
                     none}}]}}.
    {test,is_eq_exact,
          {f,20},
          [{tr,{x,0},
               {t_union,{t_atom,[nope]},
                        none,none,
                        [{{3,{t_integer,{99,99}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{99,99}},
                                     2 => {t_integer,{0,4294967295}},
                                     3 => {t_bitstring,8}}}},
                         {{3,{t_integer,{110,110}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{110,110}},
                                     2 => {t_bitstring,8},
                                     3 => {t_bitstring,8}}}}],
                        none}},
           {atom,nope}]}.
    {move,{literal,<<"ÿ">>},{x,0}}.
    {line,[{location,"test_bs.erl",86}]}.
    {call,1,{f,251}}. % test_match_clause/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     none,none,
                     [{{3,{t_integer,{99,99}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{99,99}},
                             2 => {t_integer,{0,4294967295}},
                             3 => {t_bitstring,8}}}},
                      {{3,{t_integer,{110,110}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{110,110}},
                             2 => {t_bitstring,8},
                             3 => {t_bitstring,8}}}}],
                     none}}]}}.
    {test,is_eq_exact,
          {f,19},
          [{tr,{x,0},
               {t_union,{t_atom,[nope]},
                        none,none,
                        [{{3,{t_integer,{99,99}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{99,99}},
                                     2 => {t_integer,{0,4294967295}},
                                     3 => {t_bitstring,8}}}},
                         {{3,{t_integer,{110,110}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{110,110}},
                                     2 => {t_bitstring,8},
                                     3 => {t_bitstring,8}}}}],
                        none}},
           {atom,nope}]}.
    {move,{literal,<<"n">>},{x,0}}.
    {line,[{location,"test_bs.erl",87}]}.
    {call,1,{f,251}}. % test_match_clause/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     none,none,
                     [{{3,{t_integer,{99,99}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{99,99}},
                             2 => {t_integer,{0,4294967295}},
                             3 => {t_bitstring,8}}}},
                      {{3,{t_integer,{110,110}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{110,110}},
                             2 => {t_bitstring,8},
                             3 => {t_bitstring,8}}}}],
                     none}}]}}.
    {test,is_eq_exact,
          {f,18},
          [{tr,{x,0},
               {t_union,{t_atom,[nope]},
                        none,none,
                        [{{3,{t_integer,{99,99}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{99,99}},
                                     2 => {t_integer,{0,4294967295}},
                                     3 => {t_bitstring,8}}}},
                         {{3,{t_integer,{110,110}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{110,110}},
                                     2 => {t_bitstring,8},
                                     3 => {t_bitstring,8}}}}],
                        none}},
           {atom,nope}]}.
    {move,{literal,<<110,1,2,3>>},{x,0}}.
    {line,[{location,"test_bs.erl",88}]}.
    {call,1,{f,251}}. % test_match_clause/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     none,none,
                     [{{3,{t_integer,{99,99}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{99,99}},
                             2 => {t_integer,{0,4294967295}},
                             3 => {t_bitstring,8}}}},
                      {{3,{t_integer,{110,110}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{110,110}},
                             2 => {t_bitstring,8},
                             3 => {t_bitstring,8}}}}],
                     none}}]}}.
    {test,is_eq_exact,
          {f,17},
          [{tr,{x,0},
               {t_union,{t_atom,[nope]},
                        none,none,
                        [{{3,{t_integer,{99,99}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{99,99}},
                                     2 => {t_integer,{0,4294967295}},
                                     3 => {t_bitstring,8}}}},
                         {{3,{t_integer,{110,110}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{110,110}},
                                     2 => {t_bitstring,8},
                                     3 => {t_bitstring,8}}}}],
                        none}},
           {atom,nope}]}.
    {move,{literal,<<110,1,2,3,4>>},{x,0}}.
    {line,[{location,"test_bs.erl",89}]}.
    {call,1,{f,251}}. % test_match_clause/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     none,none,
                     [{{3,{t_integer,{99,99}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{99,99}},
                             2 => {t_integer,{0,4294967295}},
                             3 => {t_bitstring,8}}}},
                      {{3,{t_integer,{110,110}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{110,110}},
                             2 => {t_bitstring,8},
                             3 => {t_bitstring,8}}}}],
                     none}}]}}.
    {test,is_eq_exact,
          {f,16},
          [{tr,{x,0},
               {t_union,{t_atom,[nope]},
                        none,none,
                        [{{3,{t_integer,{99,99}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{99,99}},
                                     2 => {t_integer,{0,4294967295}},
                                     3 => {t_bitstring,8}}}},
                         {{3,{t_integer,{110,110}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{110,110}},
                                     2 => {t_bitstring,8},
                                     3 => {t_bitstring,8}}}}],
                        none}},
           {literal,{110,<<1,2,3,4>>,<<>>}}]}.
    {move,{literal,<<110,1,2,3,4,5,6>>},{x,0}}.
    {line,[{location,"test_bs.erl",90}]}.
    {call,1,{f,251}}. % test_match_clause/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     none,none,
                     [{{3,{t_integer,{99,99}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{99,99}},
                             2 => {t_integer,{0,4294967295}},
                             3 => {t_bitstring,8}}}},
                      {{3,{t_integer,{110,110}}},
                       {t_tuple,3,true,
                           #{1 => {t_integer,{110,110}},
                             2 => {t_bitstring,8},
                             3 => {t_bitstring,8}}}}],
                     none}}]}}.
    {test,is_eq_exact,
          {f,15},
          [{tr,{x,0},
               {t_union,{t_atom,[nope]},
                        none,none,
                        [{{3,{t_integer,{99,99}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{99,99}},
                                     2 => {t_integer,{0,4294967295}},
                                     3 => {t_bitstring,8}}}},
                         {{3,{t_integer,{110,110}}},
                          {t_tuple,3,true,
                                   #{1 => {t_integer,{110,110}},
                                     2 => {t_bitstring,8},
                                     3 => {t_bitstring,8}}}}],
                        none}},
           {literal,{110,<<1,2,3,4>>,<<5,6>>}}]}.
    {move,nil,{x,1}}.
    {move,{literal,<<>>},{x,0}}.
    {line,[{location,"test_bs.erl",92}]}.
    {call,2,{f,256}}. % test_match_recursive/2
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     {t_cons,
                         {t_union,none,none,
                             {t_integer,{119,122}},
                             none,
                             {t_bitstring,8}},
                         nil},
                     none,none,none}}]}}.
    {test,is_eq_exact,
        {f,14},
        [{tr,{x,0},
             {t_union,
                 {t_atom,[nope]},
                 {t_cons,
                     {t_union,none,none,
                         {t_integer,{119,122}},
                         none,
                         {t_bitstring,8}},
                     nil},
                 none,none,none}},
         {literal,[<<>>]}]}.
    {move,nil,{x,1}}.
    {move,{literal,<<0,0,0,119>>},{x,0}}.
    {line,[{location,"test_bs.erl",93}]}.
    {call,2,{f,256}}. % test_match_recursive/2
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     {t_cons,
                         {t_union,none,none,
                             {t_integer,{119,122}},
                             none,
                             {t_bitstring,8}},
                         nil},
                     none,none,none}}]}}.
    {test,is_eq_exact,
        {f,13},
        [{tr,{x,0},
             {t_union,
                 {t_atom,[nope]},
                 {t_cons,
                     {t_union,none,none,
                         {t_integer,{119,122}},
                         none,
                         {t_bitstring,8}},
                     nil},
                 none,none,none}},
         {literal,[<<>>,119]}]}.
    {move,nil,{x,1}}.
    {move,{literal,<<122,0,0,0,119>>},{x,0}}.
    {line,[{location,"test_bs.erl",94}]}.
    {call,2,{f,256}}. % test_match_recursive/2
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     {t_cons,
                         {t_union,none,none,
                             {t_integer,{119,122}},
                             none,
                             {t_bitstring,8}},
                         nil},
                     none,none,none}}]}}.
    {test,is_eq_exact,
        {f,12},
        [{tr,{x,0},
             {t_union,
                 {t_atom,[nope]},
                 {t_cons,
                     {t_union,none,none,
                         {t_integer,{119,122}},
                         none,
                         {t_bitstring,8}},
                     nil},
                 none,none,none}},
         {literal,[<<>>,119,122]}]}.
    {move,nil,{x,1}}.
    {move,{literal,<<122,0,0,0,119,0,0,0,119,0,0,0,119,122,122>>},{x,0}}.
    {line,[{location,"test_bs.erl",95}]}.
    {call,2,{f,256}}. % test_match_recursive/2
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     {t_cons,
                         {t_union,none,none,
                             {t_integer,{119,122}},
                             none,
                             {t_bitstring,8}},
                         nil},
                     none,none,none}}]}}.
    {test,is_eq_exact,
        {f,11},
        [{tr,{x,0},
             {t_union,
                 {t_atom,[nope]},
                 {t_cons,
                     {t_union,none,none,
                         {t_integer,{119,122}},
                         none,
                         {t_bitstring,8}},
                     nil},
                 none,none,none}},
         {literal,[<<>>,122,122,119,119,119,122]}]}.
    {move,nil,{x,1}}.
    {move,{literal,<<"foo">>},{x,0}}.
    {line,[{location,"test_bs.erl",99}]}.
    {call,2,{f,256}}. % test_match_recursive/2
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[nope]},
                     {t_cons,
                         {t_union,none,none,
                             {t_integer,{119,122}},
                             none,
                             {t_bitstring,8}},
                         nil},
                     none,none,none}}]}}.
    {test,is_eq_exact,
        {f,10},
        [{tr,{x,0},
             {t_union,
                 {t_atom,[nope]},
                 {t_cons,
                     {t_union,none,none,
                         {t_integer,{119,122}},
                         none,
                         {t_bitstring,8}},
                     nil},
                 none,none,none}},
         {atom,nope}]}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.
  {label,10}.
    {badmatch,{x,0}}.
  {label,11}.
    {line,[{location,"test_bs.erl",95}]}.
    {badmatch,{x,0}}.
  {label,12}.
    {line,[{location,"test_bs.erl",94}]}.
    {badmatch,{x,0}}.
  {label,13}.
    {line,[{location,"test_bs.erl",93}]}.
    {badmatch,{x,0}}.
  {label,14}.
    {line,[{location,"test_bs.erl",92}]}.
    {badmatch,{x,0}}.
  {label,15}.
    {line,[{location,"test_bs.erl",90}]}.
    {badmatch,{x,0}}.
  {label,16}.
    {line,[{location,"test_bs.erl",89}]}.
    {badmatch,{x,0}}.
  {label,17}.
    {line,[{location,"test_bs.erl",88}]}.
    {badmatch,{x,0}}.
  {label,18}.
    {line,[{location,"test_bs.erl",87}]}.
    {badmatch,{x,0}}.
  {label,19}.
    {line,[{location,"test_bs.erl",86}]}.
    {badmatch,{x,0}}.
  {label,20}.
    {line,[{location,"test_bs.erl",85}]}.
    {badmatch,{x,0}}.
  {label,21}.
    {line,[{location,"test_bs.erl",83}]}.
    {badmatch,{x,0}}.
  {label,22}.
    {line,[{location,"test_bs.erl",79}]}.
    {badmatch,{x,0}}.
  {label,23}.
    {line,[{location,"test_bs.erl",77}]}.
    {badmatch,{x,0}}.
  {label,24}.
    {line,[{location,"test_bs.erl",75}]}.
    {badmatch,{x,0}}.
  {label,25}.
    {line,[{location,"test_bs.erl",74}]}.
    {badmatch,{x,0}}.
  {label,26}.
    {line,[{location,"test_bs.erl",73}]}.
    {badmatch,{x,0}}.
  {label,27}.
    {line,[{location,"test_bs.erl",72}]}.
    {badmatch,{x,0}}.


{function, start_3, 0, 29}.
  {label,28}.
    {line,[{location,"test_bs.erl",104}]}.
    {func_info,{atom,test_bs},{atom,start_3},0}.
  {label,29}.
    {allocate,0,0}.
    {line,[{location,"test_bs.erl",105}]}.
    {call,0,{f,31}}. % start_3a/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {call_last,0,{f,57},0}. % start_3b/0


{function, start_3a, 0, 31}.
  {label,30}.
    {line,[{location,"test_bs.erl",110}]}.
    {func_info,{atom,test_bs},{atom,start_3a},0}.
  {label,31}.
    {allocate,0,0}.
    {line,[{location,"test_bs.erl",111}]}.
    {call,0,{f,33}}. % start_3a1/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {line,[{location,"test_bs.erl",112}]}.
    {call,0,{f,36}}. % start_3a2/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {line,[{location,"test_bs.erl",113}]}.
    {call,0,{f,38}}. % start_3a3/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {line,[{location,"test_bs.erl",114}]}.
    {call,0,{f,53}}. % start_3a4/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {call_last,0,{f,55},0}. % start_3a5/0


{function, start_3a1, 0, 33}.
  {label,32}.
    {line,[{location,"test_bs.erl",119}]}.
    {func_info,{atom,test_bs},{atom,start_3a1},0}.
  {label,33}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{integer,1025},{x,0}}.
    {line,[{location,"test_bs.erl",120}]}.
    {call,1,{f,265}}. % make_binary/1
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {move,{x,0},{y,1}}.
    {move,{integer,1024},{x,2}}.
    {move,{integer,0},{x,1}}.
    {line,[{location,"test_bs.erl",121}]}.
    {call_ext,3,{extfunc,binary,part,3}}.
    {move,{x,0},{y,0}}.
    {move,{y,1},{x,1}}.
    {move,{integer,1},{x,2}}.
    {move,{x,1},{x,0}}.
    {move,{integer,1024},{x,1}}.
    {line,[{location,"test_bs.erl",122}]}.
    {call_ext,3,{extfunc,binary,part,3}}.
    {swap,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",123}]}.
    {call,1,{f,262}}. % test_match_force_gc/1
    {'%',{var_info,{x,0},
                   [{type,{t_tuple,2,true,
                                   #{1 => {t_bitstring,8},
                                     2 => {t_bitstring,8}}}}]}}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,34},[{tr,{x,1},{t_bitstring,8}},{y,0}]}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {test,is_eq_exact,{f,34},[{tr,{x,1},{t_bitstring,8}},{y,1}]}.
    {move,{integer,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,34}.
    {badmatch,{x,0}}.


{function, start_3a2, 0, 36}.
  {label,35}.
    {line,[{location,"test_bs.erl",127}]}.
    {func_info,{atom,test_bs},{atom,start_3a2},0}.
  {label,36}.
    {allocate,0,0}.
    {move,{literal,<<"bar">>},{x,1}}.
    {move,{literal,<<"foo">>},{x,0}}.
    {line,[{location,"test_bs.erl",128}]}.
    {call,2,{f,273}}. % test_put_match_string/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.


{function, start_3a3, 0, 38}.
  {label,37}.
    {line,[{location,"test_bs.erl",132}]}.
    {func_info,{atom,test_bs},{atom,start_3a3},0}.
  {label,38}.
    {allocate,0,0}.
    {line,[{location,"test_bs.erl",133}]}.
    {call,0,{f,40}}. % start_3a3_a/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {line,[{location,"test_bs.erl",134}]}.
    {call,0,{f,43}}. % start_3a3_b/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {line,[{location,"test_bs.erl",135}]}.
    {call,0,{f,46}}. % start_3a3_c/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {line,[{location,"test_bs.erl",136}]}.
    {call,0,{f,49}}. % start_3a3_d/0
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {call_last,0,{f,51},0}. % start_3a3_e/0


{function, start_3a3_a, 0, 40}.
  {label,39}.
    {line,[{location,"test_bs.erl",140}]}.
    {func_info,{atom,test_bs},{atom,start_3a3_a},0}.
  {label,40}.
    {allocate,0,0}.
    {move,{literal,<<"foobar">>},{x,1}}.
    {move,{integer,8},{x,0}}.
    {line,[{location,"test_bs.erl",141}]}.
    {call,2,{f,280}}. % skip_bits/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,8}}]}}.
    {test,is_eq_exact,
          {f,41},
          [{tr,{x,0},{t_bitstring,8}},{literal,<<"oobar">>}]}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.
  {label,41}.
    {badmatch,{x,0}}.


{function, start_3a3_b, 0, 43}.
  {label,42}.
    {line,[{location,"test_bs.erl",144}]}.
    {func_info,{atom,test_bs},{atom,start_3a3_b},0}.
  {label,43}.
    {allocate,0,0}.
    {move,{literal,<<"foobar">>},{x,1}}.
    {move,{integer,16},{x,0}}.
    {line,[{location,"test_bs.erl",145}]}.
    {call,2,{f,280}}. % skip_bits/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,8}}]}}.
    {test,is_eq_exact,
          {f,44},
          [{tr,{x,0},{t_bitstring,8}},{literal,<<"obar">>}]}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.
  {label,44}.
    {badmatch,{x,0}}.


{function, start_3a3_c, 0, 46}.
  {label,45}.
    {line,[{location,"test_bs.erl",148}]}.
    {func_info,{atom,test_bs},{atom,start_3a3_c},0}.
  {label,46}.
    {allocate,0,0}.
    {move,{literal,<<"foobar">>},{x,1}}.
    {move,{integer,48},{x,0}}.
    {line,[{location,"test_bs.erl",149}]}.
    {call,2,{f,280}}. % skip_bits/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,8}}]}}.
    {test,is_eq_exact,{f,47},[{tr,{x,0},{t_bitstring,8}},{literal,<<>>}]}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.
  {label,47}.
    {badmatch,{x,0}}.


{function, start_3a3_d, 0, 49}.
  {label,48}.
    {line,[{location,"test_bs.erl",152}]}.
    {func_info,{atom,test_bs},{atom,start_3a3_d},0}.
  {label,49}.
    {allocate_heap,0,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,458},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,8}}]}}.
    {move,{literal,{badmatch,<<"foobar">>}},{x,1}}.
    {line,[{location,"test_bs.erl",153}]}.
    {call,2,{f,218}}. % expect_error/2
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.


{function, start_3a3_e, 0, 51}.
  {label,50}.
    {line,[{location,"test_bs.erl",156}]}.
    {func_info,{atom,test_bs},{atom,start_3a3_e},0}.
  {label,51}.
    {allocate_heap,0,{alloc,[{words,0},{floats,0},{funs,2}]},0}.
    {make_fun3,{f,456},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,8}}]}}.
    {make_fun3,{f,276},0,0,{x,1},{list,[]}}.
    {'%',{var_info,{x,1},[{fun_type,{t_atom,[false,true]}}]}}.
    {line,[{location,"test_bs.erl",157}]}.
    {call,2,{f,218}}. % expect_error/2
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.


{function, start_3a4, 0, 53}.
  {label,52}.
    {line,[{location,"test_bs.erl",161}]}.
    {func_info,{atom,test_bs},{atom,start_3a4},0}.
  {label,53}.
    {allocate,0,0}.
    {line,[{location,"test_bs.erl",162}]}.
    {call,0,{f,283}}. % test_bs_match_string_unaligned/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.


{function, start_3a5, 0, 55}.
  {label,54}.
    {line,[{location,"test_bs.erl",166}]}.
    {func_info,{atom,test_bs},{atom,start_3a5},0}.
  {label,55}.
    {allocate,0,0}.
    {line,[{location,"test_bs.erl",167}]}.
    {call,0,{f,287}}. % test_match_case_type/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.


{function, start_3b, 0, 57}.
  {label,56}.
    {line,[{location,"test_bs.erl",171}]}.
    {func_info,{atom,test_bs},{atom,start_3b},0}.
  {label,57}.
    {allocate,0,0}.
    {line,[{location,"test_bs.erl",172}]}.
    {call,0,{f,295}}. % test_iterate_binary/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",174}]}.
    {call,0,{f,302}}. % test_large/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",176}]}.
    {call,0,{f,305}}. % test_copy_bits_string/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",177}]}.
    {call,0,{f,308}}. % test_bs_match_string_select/0
    {test,is_eq_exact,{f,58},[{x,0},{atom,ok}]}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.
  {label,58}.
    {badmatch,{x,0}}.


{function, start_4, 0, 60}.
  {label,59}.
    {line,[{location,"test_bs.erl",182}]}.
    {func_info,{atom,test_bs},{atom,start_4},0}.
  {label,60}.
    {allocate,0,0}.
    {line,[{location,"test_bs.erl",183}]}.
    {call,0,{f,315}}. % test_bs_skip_bits2_little/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",185}]}.
    {call,0,{f,318}}. % test_bs_variable_size_bitstring/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",186}]}.
    {call,0,{f,334}}. % test_float/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {move,{integer,0},{x,0}}.
    {deallocate,0}.
    return.


{function, test_pack_small_ints, 2, 62}.
  {label,61}.
    {line,[{location,"test_bs.erl",190}]}.
    {func_info,{atom,test_bs},{atom,test_pack_small_ints},2}.
  {label,62}.
    {'%',{var_info,{x,0},
                   [{type,{t_tuple,3,true,
                                   #{1 => {t_integer,{2,2}},
                                     2 => {t_integer,{61,61}},
                                     3 => {t_integer,{20,20}}}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_bitstring,16}}]}}.
    {line,[{location,"test_bs.erl",191}]}.
    {bs_create_bin,{f,0},
                   0,2,1,
                   {x,0},
                   {list,[{atom,string},
                          0,8,nil,
                          {string,<<23,180>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,63},
          [{tr,{x,0},{t_bitstring,1}},{tr,{x,1},{t_bitstring,16}}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,63}.
    {badmatch,{x,0}}.


{function, test_pack_integer_big_endian, 3, 65}.
  {label,64}.
    {line,[{location,"test_bs.erl",194}]}.
    {func_info,{atom,test_bs},{atom,test_pack_integer_big_endian},3}.
  {label,65}.
    {'%',{var_info,{x,0},[{type,{t_integer,{1024,1024}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{32,32}}}]}}.
    {'%',{var_info,{x,2},[{type,{t_bitstring,32}}]}}.
    {line,[{location,"test_bs.erl",195}]}.
    {bs_create_bin,{f,0},
                   0,3,1,
                   {x,0},
                   {list,[{atom,string},
                          0,8,nil,
                          {string,<<0,0,4,0>>},
                          {integer,4}]}}.
    {test,is_eq_exact,
          {f,66},
          [{tr,{x,0},{t_bitstring,1}},{tr,{x,2},{t_bitstring,32}}]}.
    {move,{x,2},{x,0}}.
    return.
  {label,66}.
    {badmatch,{x,0}}.


{function, test_pack_integers_and_binaries, 5, 68}.
  {label,67}.
    {line,[{location,"test_bs.erl",198}]}.
    {func_info,{atom,test_bs},{atom,test_pack_integers_and_binaries},5}.
  {label,68}.
    {'%',{var_info,{x,0},[{type,{t_integer,{15,15}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{2,2}}}]}}.
    {'%',{var_info,{x,2},[{type,{t_bitstring,40}}]}}.
    {'%',{var_info,{x,3},[{type,{t_bitstring,112}}]}}.
    {'%',{var_info,{x,4},[{type,{t_bitstring,112}}]}}.
    {line,[{location,"test_bs.erl",199}]}.
    {bs_create_bin,{f,0},
                   0,5,1,
                   {x,0},
                   {list,[{atom,string},
                          0,8,nil,
                          {string,<<0,0,0,15,0,2>>},
                          {integer,6},
                          {atom,binary},
                          3,8,nil,
                          {tr,{x,2},{t_bitstring,40}},
                          {atom,all},
                          {atom,binary},
                          4,8,nil,
                          {tr,{x,3},{t_bitstring,112}},
                          {integer,3}]}}.
    {test,is_eq_exact,
          {f,69},
          [{tr,{x,0},{t_bitstring,1}},{tr,{x,4},{t_bitstring,112}}]}.
    {move,{x,4},{x,0}}.
    return.
  {label,69}.
    {line,[{location,"test_bs.erl",201}]}.
    {badmatch,{x,0}}.


{function, test_unpack_integers_and_binaries, 5, 71}.
  {label,70}.
    {line,[{location,"test_bs.erl",204}]}.
    {func_info,{atom,test_bs},{atom,test_unpack_integers_and_binaries},5}.
  {label,71}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,112}},accepts_match_context]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{15,15}}}]}}.
    {'%',{var_info,{x,2},[{type,{t_integer,{2,2}}}]}}.
    {'%',{var_info,{x,3},[{type,{t_bitstring,40}}]}}.
    {'%',{var_info,{x,4},[{type,{t_bitstring,24}}]}}.
    {bs_start_match4,{atom,no_fail},5,{x,0},{x,0}}.
    {bs_get_position,{x,0},{x,1},5}.
    {test,bs_get_integer2,
          {f,72},
          5,
          [{x,0},
           {integer,32},
           1,
           {field_flags,[{anno,[205,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {test,bs_get_integer2,
          {f,72},
          5,
          [{x,0},
           {integer,16},
           1,
           {field_flags,[{anno,[205,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,5}}.
    {test,bs_get_binary2,
          {f,72},
          6,
          [{x,0},
           {integer,5},
           8,
           {field_flags,[{anno,[205,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,6}}.
    {bs_get_tail,{x,0},{x,7},7}.
    {test,is_eq_exact,
          {f,76},
          [{tr,{x,2},{t_integer,{0,4294967295}}},{integer,15}]}.
    {test,is_eq_exact,{f,75},[{tr,{x,5},{t_integer,{0,65535}}},{integer,2}]}.
    {test,is_eq_exact,
          {f,74},
          [{tr,{x,3},{t_bitstring,40}},{tr,{x,6},{t_bitstring,8}}]}.
    {test,is_eq_exact,
          {f,73},
          [{tr,{x,4},{t_bitstring,24}},{tr,{x,7},{t_bitstring,8}}]}.
    {bs_set_position,{x,0},{x,1}}.
    {bs_get_tail,{x,0},{x,0},1}.
    return.
  {label,72}.
    {bs_set_position,{x,0},{x,1}}.
    {bs_get_tail,{x,0},{x,0},1}.
    {line,[{location,"test_bs.erl",205}]}.
    {badmatch,{x,0}}.
  {label,73}.
    {line,[{location,"test_bs.erl",209}]}.
    {badmatch,{x,4}}.
  {label,74}.
    {line,[{location,"test_bs.erl",208}]}.
    {badmatch,{x,3}}.
  {label,75}.
    {line,[{location,"test_bs.erl",207}]}.
    {badmatch,{integer,2}}.
  {label,76}.
    {line,[{location,"test_bs.erl",206}]}.
    {badmatch,{integer,15}}.


{function, test_create_with_invalid_int_value, 0, 78}.
  {label,77}.
    {line,[{location,"test_bs.erl",212}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_invalid_int_value},0}.
  {label,78}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,454},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {call_only,2,{f,218}}. % expect_error/2


{function, test_create_with_invalid_int_size, 0, 80}.
  {label,79}.
    {line,[{location,"test_bs.erl",215}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_invalid_int_size},0}.
  {label,80}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,452},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {call_only,2,{f,218}}. % expect_error/2


{function, test_create_with_int_unit, 0, 82}.
  {label,81}.
    {line,[{location,"test_bs.erl",218}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_int_unit},0}.
  {label,82}.
    {allocate,0,0}.
    {move,{integer,8},{x,0}}.
    {line,[{location,"test_bs.erl",219}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,82499},{x,0}}.
    {call,2,{f,175}}. % create_int_binary_unit_3/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,86},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<1,66,67>>}]}.
    {move,{integer,16},{x,0}}.
    {line,[{location,"test_bs.erl",220}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,82499},{x,0}}.
    {call,2,{f,175}}. % create_int_binary_unit_3/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,85},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,0,1,66,67>>}]}.
    {move,{integer,32},{x,0}}.
    {line,[{location,"test_bs.erl",221}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,15},{x,0}}.
    {call,2,{f,175}}. % create_int_binary_unit_3/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,84},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,0,0,0,0,0,0,0,0,0,15>>}]}.
    {move,{integer,32},{x,0}}.
    {line,[{location,"test_bs.erl",223}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,1311768467294899695},{x,0}}.
    {line,[{location,"test_bs.erl",222}]}.
    {call,2,{f,175}}. % create_int_binary_unit_3/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,83},
          [{tr,{x,0},{t_bitstring,1}},
           {literal,<<0,0,0,0,18,52,86,120,144,171,205,239>>}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,83}.
    {badmatch,{x,0}}.
  {label,84}.
    {line,[{location,"test_bs.erl",221}]}.
    {badmatch,{x,0}}.
  {label,85}.
    {line,[{location,"test_bs.erl",220}]}.
    {badmatch,{x,0}}.
  {label,86}.
    {line,[{location,"test_bs.erl",219}]}.
    {badmatch,{x,0}}.


{function, test_create_with_unsupported_unaligned_int_size, 0, 88}.
  {label,87}.
    {line,[{location,"test_bs.erl",227}]}.
    {func_info,{atom,test_bs},
               {atom,test_create_with_unsupported_unaligned_int_size},
               0}.
  {label,88}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,450},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {call_only,1,{f,226}}. % atom_unsupported/1


{function, test_create_with_int_little_endian, 0, 90}.
  {label,89}.
    {line,[{location,"test_bs.erl",230}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_int_little_endian},0}.
  {label,90}.
    {allocate,0,0}.
    {move,{integer,16},{x,1}}.
    {move,{integer,258},{x,0}}.
    {line,[{location,"test_bs.erl",231}]}.
    {call,2,{f,179}}. % create_int_binary_little_endian/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,{f,107},[{tr,{x,0},{t_bitstring,1}},{literal,<<2,1>>}]}.
    {move,{integer,16},{x,1}}.
    {move,{integer,65534},{x,0}}.
    {line,[{location,"test_bs.erl",232}]}.
    {call,2,{f,179}}. % create_int_binary_little_endian/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,{f,106},[{tr,{x,0},{t_bitstring,1}},{literal,<<"þÿ">>}]}.
    {move,{integer,32},{x,1}}.
    {move,{integer,16909060},{x,0}}.
    {line,[{location,"test_bs.erl",233}]}.
    {call,2,{f,179}}. % create_int_binary_little_endian/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,105},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<4,3,2,1>>}]}.
    {move,{integer,32},{x,1}}.
    {move,{integer,4294901244},{x,0}}.
    {line,[{location,"test_bs.erl",234}]}.
    {call,2,{f,179}}. % create_int_binary_little_endian/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,104},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<"üýþÿ">>}]}.
    {move,{integer,8},{x,1}}.
    {move,{integer,1024},{x,0}}.
    {line,[{location,"test_bs.erl",235}]}.
    {call,2,{f,179}}. % create_int_binary_little_endian/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,{f,103},[{tr,{x,0},{t_bitstring,1}},{literal,<<0>>}]}.
    {move,{integer,258},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,8},{x,0}}.
    {line,[{location,"test_bs.erl",237}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,102},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,2,1>>}]}.
    {move,{integer,65534},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,8},{x,0}}.
    {line,[{location,"test_bs.erl",238}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,101},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,254,255>>}]}.
    {move,{integer,16909060},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,8},{x,0}}.
    {line,[{location,"test_bs.erl",239}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,100},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,4,3,2,1>>}]}.
    {move,{integer,4294901244},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,8},{x,0}}.
    {line,[{location,"test_bs.erl",240}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,99},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,252,253,254,255>>}]}.
    {move,{integer,258},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,16},{x,0}}.
    {line,[{location,"test_bs.erl",242}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,98},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,2,1>>}]}.
    {move,{integer,65534},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,16},{x,0}}.
    {line,[{location,"test_bs.erl",243}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,97},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,254,255>>}]}.
    {move,{integer,16909060},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,16},{x,0}}.
    {line,[{location,"test_bs.erl",244}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,96},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,4,3,2,1>>}]}.
    {move,{integer,4294901244},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,16},{x,0}}.
    {line,[{location,"test_bs.erl",245}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,95},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,252,253,254,255>>}]}.
    {move,{integer,258},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,24},{x,0}}.
    {line,[{location,"test_bs.erl",247}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,94},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,0,2,1>>}]}.
    {move,{integer,65534},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,24},{x,0}}.
    {line,[{location,"test_bs.erl",248}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,93},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,0,254,255>>}]}.
    {move,{integer,16909060},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,24},{x,0}}.
    {line,[{location,"test_bs.erl",249}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,92},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,0,4,3,2,1>>}]}.
    {move,{integer,4294901244},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,24},{x,0}}.
    {line,[{location,"test_bs.erl",250}]}.
    {call,3,{f,181}}. % create_int_binary_little_endian/3
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,91},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<0,0,0,252,253,254,255>>}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,91}.
    {badmatch,{x,0}}.
  {label,92}.
    {line,[{location,"test_bs.erl",249}]}.
    {badmatch,{x,0}}.
  {label,93}.
    {line,[{location,"test_bs.erl",248}]}.
    {badmatch,{x,0}}.
  {label,94}.
    {line,[{location,"test_bs.erl",247}]}.
    {badmatch,{x,0}}.
  {label,95}.
    {line,[{location,"test_bs.erl",245}]}.
    {badmatch,{x,0}}.
  {label,96}.
    {line,[{location,"test_bs.erl",244}]}.
    {badmatch,{x,0}}.
  {label,97}.
    {line,[{location,"test_bs.erl",243}]}.
    {badmatch,{x,0}}.
  {label,98}.
    {line,[{location,"test_bs.erl",242}]}.
    {badmatch,{x,0}}.
  {label,99}.
    {line,[{location,"test_bs.erl",240}]}.
    {badmatch,{x,0}}.
  {label,100}.
    {line,[{location,"test_bs.erl",239}]}.
    {badmatch,{x,0}}.
  {label,101}.
    {line,[{location,"test_bs.erl",238}]}.
    {badmatch,{x,0}}.
  {label,102}.
    {line,[{location,"test_bs.erl",237}]}.
    {badmatch,{x,0}}.
  {label,103}.
    {line,[{location,"test_bs.erl",235}]}.
    {badmatch,{x,0}}.
  {label,104}.
    {line,[{location,"test_bs.erl",234}]}.
    {badmatch,{x,0}}.
  {label,105}.
    {line,[{location,"test_bs.erl",233}]}.
    {badmatch,{x,0}}.
  {label,106}.
    {line,[{location,"test_bs.erl",232}]}.
    {badmatch,{x,0}}.
  {label,107}.
    {line,[{location,"test_bs.erl",231}]}.
    {badmatch,{x,0}}.


{function, test_create_with_int_signed, 0, 109}.
  {label,108}.
    {line,[{location,"test_bs.erl",253}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_int_signed},0}.
  {label,109}.
    {allocate,0,0}.
    {move,{integer,32},{x,1}}.
    {move,{integer,65535},{x,0}}.
    {line,[{location,"test_bs.erl",254}]}.
    {call,2,{f,183}}. % create_int_binary_signed/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {move,{x,0},{x,1}}.
    {move,{literal,<<0,0,255,255>>},{x,0}}.
    {call_last,2,{f,215},0}. % expect_equals/2


{function, test_create_with_invalid_binary_value, 0, 111}.
  {label,110}.
    {line,[{location,"test_bs.erl",257}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_invalid_binary_value},0}.
  {label,111}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,448},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {call_only,2,{f,218}}. % expect_error/2


{function, test_create_with_invalid_binary_size, 0, 113}.
  {label,112}.
    {line,[{location,"test_bs.erl",260}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_invalid_binary_size},0}.
  {label,113}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,446},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {call_only,2,{f,218}}. % expect_error/2


{function, test_create_with_binary_size_out_of_range, 0, 115}.
  {label,114}.
    {line,[{location,"test_bs.erl",263}]}.
    {func_info,{atom,test_bs},
               {atom,test_create_with_binary_size_out_of_range},
               0}.
  {label,115}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,444},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {call_only,2,{f,218}}. % expect_error/2


{function, test_create_with_unsupported_binary_unit, 0, 117}.
  {label,116}.
    {line,[{location,"test_bs.erl",266}]}.
    {func_info,{atom,test_bs},
               {atom,test_create_with_unsupported_binary_unit},
               0}.
  {label,117}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,442},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {call_only,1,{f,226}}. % atom_unsupported/1


{function, test_get_with_unsupported_int_unit, 0, 119}.
  {label,118}.
    {line,[{location,"test_bs.erl",271}]}.
    {func_info,{atom,test_bs},{atom,test_get_with_unsupported_int_unit},0}.
  {label,119}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,2}]},0}.
    {make_fun3,{f,439},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_integer,any}}]}}.
    {make_fun3,{f,437},0,0,{x,1},{list,[]}}.
    {'%',{var_info,{x,1},[{fun_type,{t_atom,[true]}}]}}.
    {call_only,2,{f,228}}. % atom_unsupported/2


{function, test_get_with_int_little_endian, 0, 121}.
  {label,120}.
    {line,[{location,"test_bs.erl",280}]}.
    {func_info,{atom,test_bs},{atom,test_get_with_int_little_endian},0}.
  {label,121}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{literal,<<"ÿþýüûúùø÷öõô">>},{x,0}}.
    {line,[{location,"test_bs.erl",281}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,1}}.
    {move,{literal,<<0,1,2,3,4,5,6,7,8,9,10,11>>},{x,0}}.
    {line,[{location,"test_bs.erl",282}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,0}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",283}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,16},{x,1}}.
    {call,2,{f,196}}. % get_integer_little_unsigned/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,141},[{tr,{x,0},{t_integer,any}},{integer,65279}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",284}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,16},{x,1}}.
    {call,2,{f,196}}. % get_integer_little_unsigned/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,140},[{tr,{x,0},{t_integer,any}},{integer,256}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",285}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,32},{x,1}}.
    {call,2,{f,196}}. % get_integer_little_unsigned/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,139},
          [{tr,{x,0},{t_integer,any}},{integer,4244504319}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",286}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,32},{x,1}}.
    {call,2,{f,196}}. % get_integer_little_unsigned/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,138},[{tr,{x,0},{t_integer,any}},{integer,50462976}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",293}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,64},{x,1}}.
    {call,2,{f,196}}. % get_integer_little_unsigned/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,137},
          [{tr,{x,0},{t_integer,any}},{integer,506097522914230528}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",295}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,136},[{tr,{x,0},{t_integer,any}},{integer,65022}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",296}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,135},[{tr,{x,0},{t_integer,any}},{integer,513}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",297}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,134},
          [{tr,{x,0},{t_integer,any}},{integer,4227661310}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",298}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,133},[{tr,{x,0},{t_integer,any}},{integer,67305985}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",300}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,132},
          [{tr,{x,0},{t_integer,any}},{integer,578437695752307201}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",302}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,131},[{tr,{x,0},{t_integer,any}},{integer,64765}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",303}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,130},[{tr,{x,0},{t_integer,any}},{integer,770}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",304}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,129},
          [{tr,{x,0},{t_integer,any}},{integer,4210818301}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",305}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,128},[{tr,{x,0},{t_integer,any}},{integer,84148994}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",307}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,127},
          [{tr,{x,0},{t_integer,any}},{integer,650777868590383874}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",309}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,126},[{tr,{x,0},{t_integer,any}},{integer,64508}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",310}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,125},[{tr,{x,0},{t_integer,any}},{integer,1027}]}.
    {move,{y,1},{x,0}}.
    {move,{y,0},{y,1}}.
    {trim,1,1}.
    {line,[{location,"test_bs.erl",311}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,124},
          [{tr,{x,0},{t_integer,any}},{integer,4193975292}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",312}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,123},
          [{tr,{x,0},{t_integer,any}},{integer,100992003}]}.
    {move,{y,0},{x,0}}.
    {trim,1,0}.
    {line,[{location,"test_bs.erl",314}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,200}}. % get_integer_little_unsigned/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,122},
          [{tr,{x,0},{t_integer,any}},{integer,723118041428460547}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,122}.
    {badmatch,{x,0}}.
  {label,123}.
    {line,[{location,"test_bs.erl",312}]}.
    {badmatch,{x,0}}.
  {label,124}.
    {line,[{location,"test_bs.erl",311}]}.
    {badmatch,{x,0}}.
  {label,125}.
    {line,[{location,"test_bs.erl",310}]}.
    {badmatch,{x,0}}.
  {label,126}.
    {line,[{location,"test_bs.erl",309}]}.
    {badmatch,{x,0}}.
  {label,127}.
    {line,[{location,"test_bs.erl",307}]}.
    {badmatch,{x,0}}.
  {label,128}.
    {line,[{location,"test_bs.erl",305}]}.
    {badmatch,{x,0}}.
  {label,129}.
    {line,[{location,"test_bs.erl",304}]}.
    {badmatch,{x,0}}.
  {label,130}.
    {line,[{location,"test_bs.erl",303}]}.
    {badmatch,{x,0}}.
  {label,131}.
    {line,[{location,"test_bs.erl",302}]}.
    {badmatch,{x,0}}.
  {label,132}.
    {line,[{location,"test_bs.erl",300}]}.
    {badmatch,{x,0}}.
  {label,133}.
    {line,[{location,"test_bs.erl",298}]}.
    {badmatch,{x,0}}.
  {label,134}.
    {line,[{location,"test_bs.erl",297}]}.
    {badmatch,{x,0}}.
  {label,135}.
    {line,[{location,"test_bs.erl",296}]}.
    {badmatch,{x,0}}.
  {label,136}.
    {line,[{location,"test_bs.erl",295}]}.
    {badmatch,{x,0}}.
  {label,137}.
    {line,[{location,"test_bs.erl",293}]}.
    {badmatch,{x,0}}.
  {label,138}.
    {line,[{location,"test_bs.erl",286}]}.
    {badmatch,{x,0}}.
  {label,139}.
    {line,[{location,"test_bs.erl",285}]}.
    {badmatch,{x,0}}.
  {label,140}.
    {line,[{location,"test_bs.erl",284}]}.
    {badmatch,{x,0}}.
  {label,141}.
    {line,[{location,"test_bs.erl",283}]}.
    {badmatch,{x,0}}.


{function, test_get_with_int_signed, 0, 143}.
  {label,142}.
    {line,[{location,"test_bs.erl",318}]}.
    {func_info,{atom,test_bs},{atom,test_get_with_int_signed},0}.
  {label,143}.
    {allocate,3,0}.
    {init_yregs,{list,[{y,0},{y,1},{y,2}]}}.
    {move,{literal,<<"ÿþýüûúùø÷öõô">>},{x,0}}.
    {line,[{location,"test_bs.erl",319}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,2}}.
    {move,{literal,<<0,1,2,3,4,5,6,7,8,9,10,11>>},{x,0}}.
    {line,[{location,"test_bs.erl",320}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,1}}.
    {move,{literal,<<128,129,130,131,132,133,134,135,136,137,138,139>>},{x,0}}.
    {line,[{location,"test_bs.erl",321}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,0}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",322}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,16},{x,1}}.
    {call,2,{f,204}}. % get_integer_big_signed/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,171},[{tr,{x,0},{t_integer,any}},{integer,-2}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",323}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,16},{x,1}}.
    {call,2,{f,204}}. % get_integer_big_signed/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,170},[{tr,{x,0},{t_integer,any}},{integer,1}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",324}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,32},{x,1}}.
    {call,2,{f,204}}. % get_integer_big_signed/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,169},[{tr,{x,0},{t_integer,any}},{integer,-66052}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",325}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,32},{x,1}}.
    {call,2,{f,204}}. % get_integer_big_signed/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,168},[{tr,{x,0},{t_integer,any}},{integer,66051}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",326}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,64},{x,1}}.
    {call,2,{f,204}}. % get_integer_big_signed/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,167},
          [{tr,{x,0},{t_integer,any}},{integer,-283686952306184}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",327}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,64},{x,1}}.
    {call,2,{f,204}}. % get_integer_big_signed/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,166},
          [{tr,{x,0},{t_integer,any}},{integer,283686952306183}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",328}]}.
    {call,1,{f,398}}. % id/1
    {move,{integer,64},{x,1}}.
    {call,2,{f,204}}. % get_integer_big_signed/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,165},
          [{tr,{x,0},{t_integer,any}},{integer,-9186918263483431289}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",330}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,164},[{tr,{x,0},{t_integer,any}},{integer,-259}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",331}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,163},[{tr,{x,0},{t_integer,any}},{integer,258}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",332}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,162},
          [{tr,{x,0},{t_integer,any}},{integer,-16909061}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",333}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,161},[{tr,{x,0},{t_integer,any}},{integer,16909060}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",334}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,160},
          [{tr,{x,0},{t_integer,any}},{integer,-72623859790382857}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",335}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,159},
          [{tr,{x,0},{t_integer,any}},{integer,72623859790382856}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",336}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,8},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,158},
          [{tr,{x,0},{t_integer,any}},{integer,-9114578090645354616}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",338}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,157},[{tr,{x,0},{t_integer,any}},{integer,-516}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",339}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,156},[{tr,{x,0},{t_integer,any}},{integer,515}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",340}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,155},
          [{tr,{x,0},{t_integer,any}},{integer,-33752070}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",341}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,154},[{tr,{x,0},{t_integer,any}},{integer,33752069}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",342}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,153},
          [{tr,{x,0},{t_integer,any}},{integer,-144964032628459530}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",343}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,152},
          [{tr,{x,0},{t_integer,any}},{integer,144964032628459529}]}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_bs.erl",344}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,16},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,151},
          [{tr,{x,0},{t_integer,any}},{integer,-9042237917807277943}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",346}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,150},[{tr,{x,0},{t_integer,any}},{integer,-773}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",347}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,16},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,149},[{tr,{x,0},{t_integer,any}},{integer,772}]}.
    {move,{y,2},{x,0}}.
    {line,[{location,"test_bs.erl",348}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,148},
          [{tr,{x,0},{t_integer,any}},{integer,-50595079}]}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",349}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,32},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,147},[{tr,{x,0},{t_integer,any}},{integer,50595078}]}.
    {move,{y,2},{x,0}}.
    {move,{y,0},{y,2}}.
    {trim,1,2}.
    {line,[{location,"test_bs.erl",350}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,146},
          [{tr,{x,0},{t_integer,any}},{integer,-217304205466536203}]}.
    {move,{y,0},{x,0}}.
    {trim,1,1}.
    {line,[{location,"test_bs.erl",351}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,145},
          [{tr,{x,0},{t_integer,any}},{integer,217304205466536202}]}.
    {move,{y,0},{x,0}}.
    {trim,1,0}.
    {line,[{location,"test_bs.erl",352}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,64},{x,2}}.
    {move,{integer,24},{x,0}}.
    {call,3,{f,208}}. % get_integer_big_signed/3
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,
          {f,144},
          [{tr,{x,0},{t_integer,any}},{integer,-8969897744969201270}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,144}.
    {badmatch,{x,0}}.
  {label,145}.
    {line,[{location,"test_bs.erl",351}]}.
    {badmatch,{x,0}}.
  {label,146}.
    {line,[{location,"test_bs.erl",350}]}.
    {badmatch,{x,0}}.
  {label,147}.
    {line,[{location,"test_bs.erl",349}]}.
    {badmatch,{x,0}}.
  {label,148}.
    {line,[{location,"test_bs.erl",348}]}.
    {badmatch,{x,0}}.
  {label,149}.
    {line,[{location,"test_bs.erl",347}]}.
    {badmatch,{x,0}}.
  {label,150}.
    {line,[{location,"test_bs.erl",346}]}.
    {badmatch,{x,0}}.
  {label,151}.
    {line,[{location,"test_bs.erl",344}]}.
    {badmatch,{x,0}}.
  {label,152}.
    {line,[{location,"test_bs.erl",343}]}.
    {badmatch,{x,0}}.
  {label,153}.
    {line,[{location,"test_bs.erl",342}]}.
    {badmatch,{x,0}}.
  {label,154}.
    {line,[{location,"test_bs.erl",341}]}.
    {badmatch,{x,0}}.
  {label,155}.
    {line,[{location,"test_bs.erl",340}]}.
    {badmatch,{x,0}}.
  {label,156}.
    {line,[{location,"test_bs.erl",339}]}.
    {badmatch,{x,0}}.
  {label,157}.
    {line,[{location,"test_bs.erl",338}]}.
    {badmatch,{x,0}}.
  {label,158}.
    {line,[{location,"test_bs.erl",336}]}.
    {badmatch,{x,0}}.
  {label,159}.
    {line,[{location,"test_bs.erl",335}]}.
    {badmatch,{x,0}}.
  {label,160}.
    {line,[{location,"test_bs.erl",334}]}.
    {badmatch,{x,0}}.
  {label,161}.
    {line,[{location,"test_bs.erl",333}]}.
    {badmatch,{x,0}}.
  {label,162}.
    {line,[{location,"test_bs.erl",332}]}.
    {badmatch,{x,0}}.
  {label,163}.
    {line,[{location,"test_bs.erl",331}]}.
    {badmatch,{x,0}}.
  {label,164}.
    {line,[{location,"test_bs.erl",330}]}.
    {badmatch,{x,0}}.
  {label,165}.
    {line,[{location,"test_bs.erl",328}]}.
    {badmatch,{x,0}}.
  {label,166}.
    {line,[{location,"test_bs.erl",327}]}.
    {badmatch,{x,0}}.
  {label,167}.
    {line,[{location,"test_bs.erl",326}]}.
    {badmatch,{x,0}}.
  {label,168}.
    {line,[{location,"test_bs.erl",325}]}.
    {badmatch,{x,0}}.
  {label,169}.
    {line,[{location,"test_bs.erl",324}]}.
    {badmatch,{x,0}}.
  {label,170}.
    {line,[{location,"test_bs.erl",323}]}.
    {badmatch,{x,0}}.
  {label,171}.
    {line,[{location,"test_bs.erl",322}]}.
    {badmatch,{x,0}}.


{function, test_get_with_unaligned_binary, 0, 173}.
  {label,172}.
    {line,[{location,"test_bs.erl",356}]}.
    {func_info,{atom,test_bs},{atom,test_get_with_unaligned_binary},0}.
  {label,173}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,2}]},0}.
    {make_fun3,{f,435},0,0,{x,0},{list,[]}}.
    {'%',
        {var_info,
            {x,0},
            [{fun_type,
                 {t_tuple,2,true,
                     #{1 => {t_integer,any},2 => {t_bitstring,8}}}}]}}.
    {make_fun3,{f,433},0,0,{x,1},{list,[]}}.
    {'%',{var_info,{x,1},[{fun_type,{t_atom,[false,true]}}]}}.
    {call_only,2,{f,228}}. % atom_unsupported/2


{function, create_int_binary_unit_3, 2, 175}.
  {label,174}.
    {line,[{location,"test_bs.erl",361}]}.
    {func_info,{atom,test_bs},{atom,create_int_binary_unit_3},2}.
  {label,175}.
    {'%',{var_info,{x,0},[{type,{t_integer,{15,1311768467294899695}}}]}}.
    {line,[{location,"test_bs.erl",362}]}.
    {bs_create_bin,{f,0},
                   0,2,1,
                   {x,0},
                   {list,[{atom,integer},
                          1,3,nil,
                          {tr,{x,0},{t_integer,{15,1311768467294899695}}},
                          {x,1}]}}.
    return.


{function, create_int_binary, 2, 177}.
  {label,176}.
    {line,[{location,"test_bs.erl",364}]}.
    {func_info,{atom,test_bs},{atom,create_int_binary},2}.
  {label,177}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[foo]},
                                   none,
                                   {t_integer,{15,65535}},
                                   none,none}}]}}.
    {line,[{location,"test_bs.erl",365}]}.
    {bs_create_bin,
        {f,0},
        0,2,1,
        {x,0},
        {list,
            [{atom,integer},
             1,1,nil,
             {tr,{x,0},
                 {t_union,
                     {t_atom,[foo]},
                     none,
                     {t_integer,{15,65535}},
                     none,none}},
             {x,1}]}}.
    return.


{function, create_int_binary_little_endian, 2, 179}.
  {label,178}.
    {line,[{location,"test_bs.erl",367}]}.
    {func_info,{atom,test_bs},{atom,create_int_binary_little_endian},2}.
  {label,179}.
    {'%',{var_info,{x,0},[{type,{t_integer,{258,4294901244}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{8,32}}}]}}.
    {line,[{location,"test_bs.erl",368}]}.
    {bs_create_bin,{f,0},
                   0,2,1,
                   {x,0},
                   {list,[{atom,integer},
                          1,1,
                          {literal,[little]},
                          {tr,{x,0},{t_integer,{258,4294901244}}},
                          {tr,{x,1},{t_integer,{8,32}}}]}}.
    return.


{function, create_int_binary_little_endian, 3, 181}.
  {label,180}.
    {line,[{location,"test_bs.erl",370}]}.
    {func_info,{atom,test_bs},{atom,create_int_binary_little_endian},3}.
  {label,181}.
    {'%',{var_info,{x,0},[{type,{t_integer,{8,24}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{258,4294901244}}}]}}.
    {'%',{var_info,{x,2},[{type,{t_integer,{16,32}}}]}}.
    {line,[{location,"test_bs.erl",371}]}.
    {bs_create_bin,{f,0},
                   0,3,1,
                   {x,0},
                   {list,[{atom,integer},
                          1,1,nil,
                          {integer,0},
                          {tr,{x,0},{t_integer,{8,24}}},
                          {atom,integer},
                          2,1,
                          {literal,[little]},
                          {tr,{x,1},{t_integer,{258,4294901244}}},
                          {tr,{x,2},{t_integer,{16,32}}}]}}.
    return.


{function, create_int_binary_signed, 2, 183}.
  {label,182}.
    {line,[{location,"test_bs.erl",373}]}.
    {func_info,{atom,test_bs},{atom,create_int_binary_signed},2}.
  {label,183}.
    {'%',{var_info,{x,0},[{type,{t_integer,{65535,65535}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{32,32}}}]}}.
    {line,[{location,"test_bs.erl",374}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,string},
                          0,8,nil,
                          {string,<<0,0,255,255>>},
                          {integer,4}]}}.
    return.


{function, create_binary_binary, 2, 185}.
  {label,184}.
    {line,[{location,"test_bs.erl",376}]}.
    {func_info,{atom,test_bs},{atom,create_binary_binary},2}.
  {label,185}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[foo]},
                                   none,none,none,
                                   {t_bitstring,24}}}]}}.
    {line,[{location,"test_bs.erl",377}]}.
    {bs_create_bin,
        {f,0},
        0,2,1,
        {x,0},
        {list,
            [{atom,binary},
             1,8,nil,
             {tr,{x,0},
                 {t_union,{t_atom,[foo]},none,none,none,{t_bitstring,24}}},
             {x,1}]}}.
    return.


{function, create_binary_binary_unit_3, 2, 187}.
  {label,186}.
    {line,[{location,"test_bs.erl",379}]}.
    {func_info,{atom,test_bs},{atom,create_binary_binary_unit_3},2}.
  {label,187}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,24}}]}}.
    {line,[{location,"test_bs.erl",380}]}.
    {bs_create_bin,{f,0},
                   0,2,1,
                   {x,0},
                   {list,[{atom,binary},
                          1,3,nil,
                          {tr,{x,0},{t_bitstring,24}},
                          {x,1}]}}.
    return.


{function, get_integer_big_unsigned, 2, 189}.
  {label,188}.
    {line,[{location,"test_bs.erl",382}]}.
    {func_info,{atom,test_bs},{atom,get_integer_big_unsigned},2}.
  {label,189}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,8}},accepts_match_context]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{8,32}}}]}}.
    {bs_start_match4,{atom,no_fail},2,{x,0},{x,0}}.
    {bs_get_position,{x,0},{x,2},2}.
    {test,bs_get_integer2,
          {f,190},
          3,
          [{x,0},
           {x,1},
           1,
           {field_flags,[{anno,[383,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,1}}.
    {test,bs_test_unit,{f,190},[{x,0},8]}.
    {move,{x,1},{x,0}}.
    return.
  {label,190}.
    {bs_set_position,{x,0},{x,2}}.
    {bs_get_tail,{x,0},{x,0},1}.
    {line,[{location,"test_bs.erl",383}]}.
    {badmatch,{x,0}}.


{function, get_integer_big_unsigned_unit_3, 2, 192}.
  {label,191}.
    {line,[{location,"test_bs.erl",386}]}.
    {func_info,{atom,test_bs},{atom,get_integer_big_unsigned_unit_3},2}.
  {label,192}.
    {'%',{var_info,{x,0},[accepts_match_context]}}.
    {test,bs_start_match3,{f,194},2,[{x,0}],{x,2}}.
    {bs_get_position,{x,2},{x,0},3}.
    {test,bs_get_integer2,
          {f,193},
          3,
          [{x,2},
           {x,1},
           1,
           {field_flags,[{anno,[387,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,1}}.
    {test,bs_test_unit,{f,193},[{x,2},3]}.
    {move,{x,1},{x,0}}.
    return.
  {label,193}.
    {bs_set_position,{x,2},{x,0}}.
    {bs_get_tail,{x,2},{x,0},3}.
    {line,[{location,"test_bs.erl",387}]}.
    {badmatch,{x,0}}.
  {label,194}.
    {badmatch,{x,0}}.


{function, get_integer_little_unsigned, 2, 196}.
  {label,195}.
    {line,[{location,"test_bs.erl",390}]}.
    {func_info,{atom,test_bs},{atom,get_integer_little_unsigned},2}.
  {label,196}.
    {'%',{var_info,{x,0},[accepts_match_context]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{16,64}}}]}}.
    {test,bs_start_match3,{f,198},2,[{x,0}],{x,2}}.
    {bs_get_position,{x,2},{x,0},3}.
    {test,bs_get_integer2,
          {f,197},
          3,
          [{x,2},
           {x,1},
           1,
           {field_flags,[{anno,[391,{file,"test_bs.erl"}]},unsigned,little]}],
          {x,1}}.
    {test,bs_test_unit,{f,197},[{x,2},8]}.
    {move,{x,1},{x,0}}.
    return.
  {label,197}.
    {bs_set_position,{x,2},{x,0}}.
    {bs_get_tail,{x,2},{x,0},3}.
    {line,[{location,"test_bs.erl",391}]}.
    {badmatch,{x,0}}.
  {label,198}.
    {badmatch,{x,0}}.


{function, get_integer_little_unsigned, 3, 200}.
  {label,199}.
    {line,[{location,"test_bs.erl",394}]}.
    {func_info,{atom,test_bs},{atom,get_integer_little_unsigned},3}.
  {label,200}.
    {'%',{var_info,{x,0},[{type,{t_integer,{8,24}}}]}}.
    {'%',{var_info,{x,1},[accepts_match_context]}}.
    {'%',{var_info,{x,2},[{type,{t_integer,{16,64}}}]}}.
    {test,bs_start_match3,{f,202},3,[{x,1}],{x,3}}.
    {bs_get_position,{x,3},{x,1},4}.
    {test,bs_skip_bits2,
          {f,201},
          [{x,3},
           {x,0},
           1,
           {field_flags,[{anno,[395,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_get_integer2,
          {f,201},
          4,
          [{x,3},
           {x,2},
           1,
           {field_flags,[{anno,[395,{file,"test_bs.erl"}]},unsigned,little]}],
          {x,0}}.
    {test,bs_test_unit,{f,201},[{x,3},8]}.
    return.
  {label,201}.
    {bs_set_position,{x,3},{x,1}}.
    {bs_get_tail,{x,3},{x,0},4}.
    {line,[{location,"test_bs.erl",395}]}.
    {badmatch,{x,0}}.
  {label,202}.
    {badmatch,{x,1}}.


{function, get_integer_big_signed, 2, 204}.
  {label,203}.
    {line,[{location,"test_bs.erl",398}]}.
    {func_info,{atom,test_bs},{atom,get_integer_big_signed},2}.
  {label,204}.
    {'%',{var_info,{x,0},[accepts_match_context]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{16,64}}}]}}.
    {test,bs_start_match3,{f,206},2,[{x,0}],{x,2}}.
    {bs_get_position,{x,2},{x,0},3}.
    {test,bs_get_integer2,
          {f,205},
          3,
          [{x,2},
           {x,1},
           1,
           {field_flags,[{anno,[399,{file,"test_bs.erl"}]},signed,big]}],
          {x,1}}.
    {test,bs_test_unit,{f,205},[{x,2},8]}.
    {move,{x,1},{x,0}}.
    return.
  {label,205}.
    {bs_set_position,{x,2},{x,0}}.
    {bs_get_tail,{x,2},{x,0},3}.
    {line,[{location,"test_bs.erl",399}]}.
    {badmatch,{x,0}}.
  {label,206}.
    {badmatch,{x,0}}.


{function, get_integer_big_signed, 3, 208}.
  {label,207}.
    {line,[{location,"test_bs.erl",402}]}.
    {func_info,{atom,test_bs},{atom,get_integer_big_signed},3}.
  {label,208}.
    {'%',{var_info,{x,0},[{type,{t_integer,{8,24}}}]}}.
    {'%',{var_info,{x,1},[accepts_match_context]}}.
    {'%',{var_info,{x,2},[{type,{t_integer,{16,64}}}]}}.
    {test,bs_start_match3,{f,210},3,[{x,1}],{x,3}}.
    {bs_get_position,{x,3},{x,1},4}.
    {test,bs_skip_bits2,
          {f,209},
          [{x,3},
           {x,0},
           1,
           {field_flags,[{anno,[403,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_get_integer2,
          {f,209},
          4,
          [{x,3},
           {x,2},
           1,
           {field_flags,[{anno,[403,{file,"test_bs.erl"}]},signed,big]}],
          {x,0}}.
    {test,bs_test_unit,{f,209},[{x,3},8]}.
    return.
  {label,209}.
    {bs_set_position,{x,3},{x,1}}.
    {bs_get_tail,{x,3},{x,0},4}.
    {line,[{location,"test_bs.erl",403}]}.
    {badmatch,{x,0}}.
  {label,210}.
    {badmatch,{x,1}}.


{function, get_int_then_binary, 3, 212}.
  {label,211}.
    {line,[{location,"test_bs.erl",406}]}.
    {func_info,{atom,test_bs},{atom,get_int_then_binary},3}.
  {label,212}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,32}},accepts_match_context]}}.
    {bs_start_match4,{atom,no_fail},3,{x,0},{x,0}}.
    {bs_get_position,{x,0},{x,3},3}.
    {test,bs_get_integer2,
          {f,213},
          4,
          [{x,0},
           {x,1},
           1,
           {field_flags,[{anno,[407,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,1}}.
    {test,bs_get_binary2,
          {f,213},
          4,
          [{x,0},
           {x,2},
           8,
           {field_flags,[{anno,[407,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {test_heap,3,3}.
    {put_tuple2,{x,0},{list,[{x,1},{x,2}]}}.
    return.
  {label,213}.
    {bs_set_position,{x,0},{x,3}}.
    {bs_get_tail,{x,0},{x,0},1}.
    {line,[{location,"test_bs.erl",407}]}.
    {badmatch,{x,0}}.


{function, expect_equals, 2, 215}.
  {label,214}.
    {line,[{location,"test_bs.erl",410}]}.
    {func_info,{atom,test_bs},{atom,expect_equals},2}.
  {label,215}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,32}}]}}.
    {'%',{var_info,{x,1},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,216},
          [{tr,{x,1},{t_bitstring,1}},{tr,{x,0},{t_bitstring,32}}]}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,216}.
    {test_heap,4,2}.
    {put_tuple2,{x,0},{list,[{atom,not_equal},{x,0},{x,1}]}}.
    {line,[{location,"test_bs.erl",413}]}.
    {call_ext_only,1,{extfunc,erlang,throw,1}}.


{function, expect_error, 2, 218}.
  {label,217}.
    {line,[{location,"test_bs.erl",415}]}.
    {func_info,{atom,test_bs},{atom,expect_error},2}.
  {label,218}.
    {'%',{var_info,{x,0},[{type,{t_fun,0,any,{t_bs_matchable,1}}}]}}.
    {'%',
        {var_info,
            {x,1},
            [{type,
                 {t_union,
                     {t_atom,[badarg]},
                     none,none,
                     [{{2,{t_atom,[badmatch]}},
                       {t_tuple,2,true,
                           #{1 => {t_atom,[badmatch]},
                             2 => {t_bitstring,16}}}}],
                     {t_fun,2,any,{t_atom,[false,true]}}}}]}}.
    {test,is_atom,
          {f,219},
          [{tr,{x,1},
               {t_union,{t_atom,[badarg]},
                        none,none,
                        [{{2,{t_atom,[badmatch]}},
                          {t_tuple,2,true,
                                   #{1 => {t_atom,[badmatch]},
                                     2 => {t_bitstring,16}}}}],
                        {t_fun,2,any,{t_atom,[false,true]}}}}]}.
    {jump,{f,220}}.
  {label,219}.
    {test,is_tuple,
          {f,221},
          [{tr,{x,1},
               {t_union,none,none,none,
                        [{{2,{t_atom,[badmatch]}},
                          {t_tuple,2,true,
                                   #{1 => {t_atom,[badmatch]},
                                     2 => {t_bitstring,16}}}}],
                        {t_fun,2,any,{t_atom,[false,true]}}}}]}.
  {label,220}.
    {test_heap,{alloc,[{words,1},{floats,0},{funs,1}]},2}.
    {make_fun3,{f,430},0,0,{x,1},{list,[{x,1}]}}.
    {'%',{var_info,{x,1},[{fun_type,{t_atom,[false,true]}}]}}.
    {call_only,2,{f,218}}. % expect_error/2
  {label,221}.
    {allocate,4,2}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{x,1},{y,2}}.
    {'try',{y,3},{f,222}}.
    {line,[{scope,[3]},{location,"test_bs.erl",420}]}.
    {call_fun2,{atom,safe},0,{tr,{x,0},{t_fun,0,any,{t_bs_matchable,1}}}}.
    {'%',{var_info,{x,0},[{type,{t_bs_matchable,1}}]}}.
    {try_end,{y,3}}.
    {move,{atom,unexpected},{x,0}}.
    {jump,{f,224}}.
  {label,222}.
    {try_case,{y,3}}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{y,0},{x,1}}.
    {move,{y,2},{x,2}}.
    {init_yregs,{list,[{y,2}]}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",424}]}.
    {call_fun2,{atom,safe},2,{tr,{x,2},{t_fun,2,any,{t_atom,[false,true]}}}}.
    {'%',{var_info,{x,0},[{type,{t_atom,[false,true]}}]}}.
    {test,is_eq_exact,{f,223},[{x,0},{atom,true}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,4}.
    return.
  {label,223}.
    {test_heap,3,0}.
    {put_tuple2,{x,0},{list,[{y,1},{y,0}]}}.
    {line,[{scope,[0]},{location,"test_bs.erl",426}]}.
    {call_ext,1,{extfunc,erlang,display,1}}.
    {test_heap,7,0}.
    {put_tuple2,{x,0},{list,[{y,1},{y,0}]}}.
    {put_tuple2,{x,0},{list,[{atom,got},{x,0},{atom,validator_failed}]}}.
  {label,224}.
    {line,[{scope,[0,1]},{location,"test_bs.erl",418}]}.
    {badmatch,{x,0}}.


{function, atom_unsupported, 1, 226}.
  {label,225}.
    {line,[{location,"test_bs.erl",433}]}.
    {func_info,{atom,test_bs},{atom,atom_unsupported},1}.
  {label,226}.
    {'%',{var_info,{x,0},[{type,{t_fun,0,any,{t_bitstring,1}}}]}}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},1}.
    {make_fun3,{f,428},0,0,{x,1},{list,[]}}.
    {'%',{var_info,{x,1},[{fun_type,{t_atom,[false,true]}}]}}.
    {call_only,2,{f,228}}. % atom_unsupported/2


{function, atom_unsupported, 2, 228}.
  {label,227}.
    {line,[{location,"test_bs.erl",436}]}.
    {func_info,{atom,test_bs},{atom,atom_unsupported},2}.
  {label,228}.
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_fun,0,any,
                     {t_union,none,none,
                         {t_integer,any},
                         {t_tuple,2,true,
                             #{1 => {t_integer,any},2 => {t_bitstring,8}}},
                         {t_bitstring,1}}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_fun,1,any,{t_atom,[false,true]}}}]}}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {'try',{y,1},{f,229}}.
    {line,[{location,"test_bs.erl",439}]}.
    {call_fun2,
        {atom,safe},
        0,
        {tr,{x,0},
            {t_fun,0,any,
                {t_union,none,none,
                    {t_integer,any},
                    {t_tuple,2,true,
                        #{1 => {t_integer,any},2 => {t_bitstring,8}}},
                    {t_bitstring,1}}}}}.
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,none,none,
                     {t_integer,any},
                     {t_tuple,2,true,
                         #{1 => {t_integer,any},2 => {t_bitstring,8}}},
                     {t_bitstring,1}}}]}}.
    {try_end,{y,1}}.
    {move,{atom,ok},{y,1}}.
    {jump,{f,230}}.
  {label,229}.
    {try_case,{y,1}}.
    {test,is_eq_exact,
          {f,233},
          [{tr,{x,0},{t_atom,[error,exit,throw]}},{atom,error}]}.
    {move,{x,1},{y,1}}.
  {label,230}.
    {move,{atom,machine},{x,0}}.
    {line,[{scope,[0,1]},{location,"test_bs.erl",444}]}.
    {call_ext,1,{extfunc,erlang,system_info,1}}.
    {test,is_eq_exact,{f,231},[{x,0},{literal,"BEAM"}]}.
    {test,is_eq_exact,{f,234},[{y,1},{atom,ok}]}.
    {jump,{f,232}}.
  {label,231}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {line,[{scope,[0]},{location,"test_bs.erl",448}]}.
    {call_fun2,{atom,safe},1,{tr,{x,1},{t_fun,1,any,{t_atom,[false,true]}}}}.
    {'%',{var_info,{x,0},[{type,{t_atom,[false,true]}}]}}.
    {test,is_eq_exact,{f,235},[{x,0},{atom,true}]}.
  {label,232}.
    {move,{atom,ok},{x,0}}.
    {deallocate,2}.
    return.
  {label,233}.
    {bif,raise,{f,0},[{x,2},{x,1}],{x,0}}.
  {label,234}.
    {line,[{scope,[0]},{location,"test_bs.erl",446}]}.
    {badmatch,{atom,ok}}.
  {label,235}.
    {line,[{scope,[0]},{location,"test_bs.erl",448}]}.
    {badmatch,{atom,false}}.


{function, test_match_first_integer, 1, 237}.
  {label,236}.
    {line,[{location,"test_bs.erl",452}]}.
    {func_info,{atom,test_bs},{atom,test_match_first_integer},1}.
  {label,237}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,8}},accepts_match_context]}}.
    {bs_start_match4,{atom,no_fail},1,{x,0},{x,0}}.
    {test,bs_get_integer2,
          {f,240},
          1,
          [{x,0},
           {integer,8},
           1,
           {field_flags,[{anno,[452,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,1}}.
    {select_val,{x,1},
                {f,240},
                {list,[{integer,171},{f,239},{integer,255},{f,238}]}}.
  {label,238}.
    {bs_get_tail,{x,0},{x,0},1}.
    return.
  {label,239}.
    {test,bs_match_string,{f,240},[{x,0},8,{string,<<"Í">>}]}.
    {bs_get_tail,{x,0},{x,0},1}.
    return.
  {label,240}.
    {move,{atom,nope},{x,0}}.
    return.


{function, test_bs_append, 2, 242}.
  {label,241}.
    {line,[{location,"test_bs.erl",459}]}.
    {func_info,{atom,test_bs},{atom,test_bs_append},2}.
  {label,242}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,24}}]}}.
    {'%',{var_info,{x,1},[{type,{t_bitstring,48}}]}}.
    {line,[{location,"test_bs.erl",460}]}.
    {bs_create_bin,{f,0},
                   0,2,8,
                   {x,0},
                   {list,[{atom,append},
                          1,8,nil,
                          {tr,{x,0},{t_bitstring,24}},
                          {atom,all},
                          {atom,binary},
                          2,8,nil,
                          {tr,{x,1},{t_bitstring,48}},
                          {atom,all}]}}.
    return.


{function, test_bs_private_append, 1, 244}.
  {label,243}.
    {line,[{location,"test_bs.erl",462}]}.
    {func_info,{atom,test_bs},{atom,test_bs_private_append},1}.
  {label,244}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,24}},accepts_match_context]}}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {move,{integer,256},{x,0}}.
    bs_init_writable.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {call_last,2,{f,425},1}. % '-test_bs_private_append/1-lbc$^0/2-0-'/2


{function, test_bs_private_append2, 2, 246}.
  {label,245}.
    {line,[{location,"test_bs.erl",466}]}.
    {func_info,{atom,test_bs},{atom,test_bs_private_append2},2}.
  {label,246}.
    {'%',{var_info,{x,0},[accepts_match_context]}}.
    {'%',{var_info,{x,1},[{type,{t_bitstring,1}}]}}.
    {test,bs_start_match3,{f,245},2,[{x,0}],{x,2}}.
    {bs_get_position,{x,2},{x,0},3}.
    {test,bs_get_integer2,
          {f,248},
          3,
          [{x,2},
           {integer,8},
           1,
           {field_flags,[{anno,[466,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,3}}.
    {test,bs_get_integer2,
          {f,248},
          4,
          [{x,2},
           {integer,8},
           1,
           {field_flags,[{anno,[466,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,4}}.
    {bs_get_position,{x,2},{x,5},5}.
    {test,bs_get_integer2,
          {f,247},
          6,
          [{x,2},
           {integer,8},
           1,
           {field_flags,[{anno,[466,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,6}}.
    {test,bs_get_integer2,
          {f,247},
          7,
          [{x,2},
           {integer,8},
           1,
           {field_flags,[{anno,[466,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,7}}.
    {test,bs_test_unit,{f,247},[{x,2},8]}.
    {gc_bif,byte_size,{f,0},8,[{tr,{x,1},{t_bitstring,1}}],{x,8}}.
    {test,is_lt,
          {f,247},
          [{tr,{x,8},{t_integer,{0,288230376151711743}}},{integer,16}]}.
    {line,[{location,"test_bs.erl",469}]}.
    {bs_create_bin,{f,0},
                   0,8,8,
                   {x,1},
                   {list,[{atom,append},
                          1,8,nil,
                          {tr,{x,1},{t_bitstring,1}},
                          {atom,all},
                          {atom,integer},
                          2,1,nil,
                          {tr,{x,3},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          3,1,nil,
                          {tr,{x,4},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          4,1,nil,
                          {tr,{x,6},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          5,1,nil,
                          {tr,{x,7},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          6,1,nil,
                          {tr,{x,3},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          7,1,nil,
                          {tr,{x,4},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          8,1,nil,
                          {tr,{x,6},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          9,1,nil,
                          {tr,{x,7},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          10,1,nil,
                          {tr,{x,3},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          11,1,nil,
                          {tr,{x,4},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          12,1,nil,
                          {tr,{x,6},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          13,1,nil,
                          {tr,{x,7},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          14,1,nil,
                          {tr,{x,3},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          15,1,nil,
                          {tr,{x,4},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          16,1,nil,
                          {tr,{x,6},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          17,1,nil,
                          {tr,{x,7},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          18,1,nil,
                          {tr,{x,3},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          19,1,nil,
                          {tr,{x,4},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          20,1,nil,
                          {tr,{x,6},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          21,1,nil,
                          {tr,{x,7},{t_integer,{0,255}}},
                          {integer,4}]}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,246}}. % test_bs_private_append2/2
  {label,247}.
    {bs_set_position,{x,2},{x,5}}.
    {test,bs_test_unit,{f,249},[{x,2},8]}.
    {line,[{location,"test_bs.erl",473}]}.
    {bs_create_bin,{f,0},
                   0,5,8,
                   {x,1},
                   {list,[{atom,append},
                          1,8,nil,
                          {tr,{x,1},{t_bitstring,1}},
                          {atom,all},
                          {atom,integer},
                          2,1,nil,
                          {tr,{x,3},{t_integer,{0,255}}},
                          {integer,4},
                          {atom,integer},
                          3,1,nil,
                          {tr,{x,4},{t_integer,{0,255}}},
                          {integer,4}]}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,246}}. % test_bs_private_append2/2
  {label,248}.
    {bs_set_position,{x,2},{x,0}}.
    {test,bs_test_tail2,{f,249},[{x,2},0]}.
    {move,{x,1},{x,0}}.
    return.
  {label,249}.
    {bs_set_position,{x,2},{x,0}}.
    {bs_get_tail,{x,2},{x,0},3}.
    {jump,{f,245}}.


{function, test_match_clause, 1, 251}.
  {label,250}.
    {line,[{location,"test_bs.erl",477}]}.
    {func_info,{atom,test_bs},{atom,test_match_clause},1}.
  {label,251}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,8}},accepts_match_context]}}.
    {bs_start_match4,{atom,no_fail},1,{x,0},{x,0}}.
    {test,bs_get_integer2,
          {f,254},
          1,
          [{x,0},
           {integer,8},
           1,
           {field_flags,[{anno,[477,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,1}}.
    {select_val,{x,1},
                {f,254},
                {list,[{integer,99},{f,253},{integer,110},{f,252}]}}.
  {label,252}.
    {test,bs_get_binary2,
          {f,254},
          1,
          [{x,0},
           {integer,4},
           8,
           {field_flags,[{anno,[477,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,1}}.
    {bs_get_tail,{x,0},{x,0},2}.
    {test_heap,4,2}.
    {put_tuple2,{x,0},{list,[{integer,110},{x,1},{x,0}]}}.
    return.
  {label,253}.
    {test,bs_get_integer2,
          {f,254},
          1,
          [{x,0},
           {integer,32},
           1,
           {field_flags,[{anno,[481,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,1}}.
    {test,bs_get_binary2,
          {f,254},
          2,
          [{x,0},
           {integer,16},
           8,
           {field_flags,[{anno,[481,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {test,bs_test_tail2,{f,254},[{x,0},0]}.
    {test_heap,4,3}.
    {put_tuple2,{x,0},{list,[{integer,99},{x,1},{x,2}]}}.
    return.
  {label,254}.
    {move,{atom,nope},{x,0}}.
    return.


{function, test_match_recursive, 2, 256}.
  {label,255}.
    {line,[{location,"test_bs.erl",488}]}.
    {func_info,{atom,test_bs},{atom,test_match_recursive},2}.
  {label,256}.
    {'%',{var_info,{x,0},[{type,{t_bs_matchable,8}},accepts_match_context]}}.
    {'%',{var_info,{x,1},[{type,{t_list,{t_integer,{119,122}},nil}}]}}.
    {bs_start_match4,{atom,no_fail},2,{x,0},{x,0}}.
    {test,bs_test_tail2,{f,257},[{x,0},0]}.
    {bs_get_tail,{x,0},{x,0},2}.
    {test_heap,2,2}.
    {put_list,{x,0},{x,1},{x,0}}.
    return.
  {label,257}.
    {test,bs_get_integer2,
          {f,260},
          2,
          [{x,0},
           {integer,8},
           1,
           {field_flags,[{anno,[490,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {select_val,{x,2},
                {f,260},
                {list,[{integer,0},{f,259},{integer,122},{f,258}]}}.
  {label,258}.
    {test_heap,2,2}.
    {put_list,{integer,122},{x,1},{x,1}}.
    {call_only,2,{f,256}}. % test_match_recursive/2
  {label,259}.
    {test,bs_match_string,{f,260},[{x,0},24,{string,<<0,0,119>>}]}.
    {test_heap,2,2}.
    {put_list,{integer,119},{x,1},{x,1}}.
    {call_only,2,{f,256}}. % test_match_recursive/2
  {label,260}.
    {move,{atom,nope},{x,0}}.
    return.


{function, test_match_force_gc, 1, 262}.
  {label,261}.
    {line,[{location,"test_bs.erl",497}]}.
    {func_info,{atom,test_bs},{atom,test_match_force_gc},1}.
  {label,262}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}},accepts_match_context]}}.
    {bs_start_match4,{atom,no_fail},1,{x,0},{x,0}}.
    {bs_get_position,{x,0},{x,1},1}.
    {test,bs_get_binary2,
          {f,263},
          2,
          [{x,0},
           {integer,1024},
           8,
           {field_flags,[{anno,[497,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {test,bs_get_binary2,
          {f,263},
          3,
          [{x,0},
           {atom,all},
           8,
           {field_flags,[{anno,[497,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,3}}.
    {test_heap,3,4}.
    {put_tuple2,{x,0},{list,[{x,2},{x,3}]}}.
    return.
  {label,263}.
    {bs_set_position,{x,0},{x,1}}.
    {bs_get_tail,{x,0},{x,0},1}.
    {jump,{f,261}}.


{function, make_binary, 1, 265}.
  {label,264}.
    {line,[{location,"test_bs.erl",500}]}.
    {func_info,{atom,test_bs},{atom,make_binary},1}.
  {label,265}.
    {'%',{var_info,{x,0},[{type,{t_integer,{1025,1025}}}]}}.
    {move,{literal,<<>>},{x,1}}.
    {call_only,2,{f,267}}. % make_binary/2


{function, make_binary, 2, 267}.
  {label,266}.
    {line,[{location,"test_bs.erl",503}]}.
    {func_info,{atom,test_bs},{atom,make_binary},2}.
  {label,267}.
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {'%',{var_info,{x,1},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,{f,268},[{tr,{x,0},{t_integer,any}},{integer,0}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,268}.
    {line,[{location,"test_bs.erl",506}]}.
    {gc_bif,'rem',{f,0},2,[{tr,{x,0},{t_integer,any}},{integer,256}],{x,2}}.
    {line,[{location,"test_bs.erl",507}]}.
    {gc_bif,'-',{f,0},3,[{tr,{x,0},{t_integer,any}},{integer,1}],{x,0}}.
    {bs_create_bin,{f,0},
                   0,3,8,
                   {x,1},
                   {list,[{atom,append},
                          1,8,nil,
                          {tr,{x,1},{t_bitstring,1}},
                          {atom,all},
                          {atom,integer},
                          2,1,nil,
                          {tr,{x,2},{t_integer,{-255,255}}},
                          {integer,8}]}}.
    {call_only,2,{f,267}}. % make_binary/2


{function, make_binary_copy, 3, 270}.
  {label,269}.
    {line,[{location,"test_bs.erl",509}]}.
    {func_info,{atom,test_bs},{atom,make_binary_copy},3}.
  {label,270}.
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{0,240}}}]}}.
    {'%',{var_info,{x,2},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,{f,271},[{tr,{x,0},{t_integer,any}},{integer,0}]}.
    {move,{x,2},{x,0}}.
    return.
  {label,271}.
    {line,[{location,"test_bs.erl",512}]}.
    {gc_bif,'-',{f,0},3,[{tr,{x,0},{t_integer,any}},{integer,1}],{x,0}}.
    {bs_create_bin,{f,0},
                   0,3,1,
                   {x,2},
                   {list,[{atom,integer},
                          1,1,nil,
                          {tr,{x,1},{t_integer,{0,240}}},
                          {integer,8},
                          {atom,binary},
                          2,8,nil,
                          {tr,{x,2},{t_bitstring,1}},
                          {atom,all}]}}.
    {call_only,3,{f,270}}. % make_binary_copy/3


{function, test_put_match_string, 2, 273}.
  {label,272}.
    {line,[{location,"test_bs.erl",514}]}.
    {func_info,{atom,test_bs},{atom,test_put_match_string},2}.
  {label,273}.
    {'%',{var_info,{x,0},[{type,{t_bitstring,24}}]}}.
    {'%',{var_info,{x,1},[{type,{t_bitstring,24}}]}}.
    {line,[{location,"test_bs.erl",515}]}.
    {bs_create_bin,{f,0},
                   0,2,1,
                   {x,1},
                   {list,[{atom,string},
                          0,8,nil,
                          {string,<<"foo">>},
                          {integer,3},
                          {atom,binary},
                          4,8,nil,
                          {tr,{x,1},{t_bitstring,24}},
                          {atom,all}]}}.
    {bs_start_match4,{atom,no_fail},2,{x,1},{x,2}}.
    {test,bs_get_binary2,
          {f,274},
          3,
          [{x,2},
           {integer,3},
           8,
           {field_flags,[{anno,[516,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,3}}.
    {test,bs_match_string,{f,274},[{x,2},24,{string,<<"bar">>}]}.
    {test,bs_test_tail2,{f,274},[{x,2},0]}.
    {test,is_eq_exact,
          {f,274},
          [{tr,{x,3},{t_bitstring,8}},{tr,{x,0},{t_bitstring,24}}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,274}.
    {line,[{location,"test_bs.erl",516}]}.
    {badmatch,{x,1}}.


{function, skip_bits_unsupported, 2, 276}.
  {label,275}.
    {line,[{location,"test_bs.erl",526}]}.
    {func_info,{atom,test_bs},{atom,skip_bits_unsupported},2}.
  {label,276}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{atom,machine},{x,0}}.
    {line,[{location,"test_bs.erl",527}]}.
    {call_ext,1,{extfunc,erlang,system_info,1}}.
    {test,is_eq_exact,{f,278},[{x,0},{literal,"BEAM"}]}.
    {test,is_eq_exact,{f,277},[{y,1},{atom,error}]}.
    {bif,'=:=',{f,0},[{y,0},{literal,{badmatch,<<"foobar">>}}],{x,0}}.
    {deallocate,2}.
    return.
  {label,277}.
    {move,{atom,false},{x,0}}.
    {deallocate,2}.
    return.
  {label,278}.
    {move,{atom,true},{x,0}}.
    {deallocate,2}.
    return.


{function, skip_bits, 2, 280}.
  {label,279}.
    {line,[{location,"test_bs.erl",533}]}.
    {func_info,{atom,test_bs},{atom,skip_bits},2}.
  {label,280}.
    {'%',{var_info,{x,0},[{type,{t_integer,{1,128}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_bitstring,48}},accepts_match_context]}}.
    {bs_start_match4,{atom,no_fail},2,{x,1},{x,1}}.
    {bs_get_position,{x,1},{x,2},2}.
    {test,bs_skip_bits2,
          {f,281},
          [{x,1},
           {x,0},
           1,
           {field_flags,[{anno,[534,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_get_binary2,
          {f,281},
          3,
          [{x,1},
           {atom,all},
           8,
           {field_flags,[{anno,[534,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,0}}.
    return.
  {label,281}.
    {bs_set_position,{x,1},{x,2}}.
    {bs_get_tail,{x,1},{x,0},2}.
    {line,[{location,"test_bs.erl",534}]}.
    {badmatch,{x,0}}.


{function, test_bs_match_string_unaligned, 0, 283}.
  {label,282}.
    {line,[{location,"test_bs.erl",537}]}.
    {func_info,{atom,test_bs},{atom,test_bs_match_string_unaligned},0}.
  {label,283}.
    {allocate,0,0}.
    {move,{literal,<<5,64,42>>},{x,0}}.
    {line,[{location,"test_bs.erl",538}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,285},1,[{x,0}],{x,1}}.
    {test,bs_match_string,{f,285},[{x,1},1,{string,<<0>>}]}.
    {test,bs_skip_bits2,
          {f,285},
          [{x,1},
           {integer,3},
           1,
           {field_flags,[{anno,[538,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_match_string,{f,285},[{x,1},7,{string,<<"T">>}]}.
    {test,bs_skip_bits2,
          {f,285},
          [{x,1},
           {integer,5},
           1,
           {field_flags,[{anno,[538,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_match_string,{f,285},[{x,1},8,{string,<<"*">>}]}.
    {test,bs_test_tail2,{f,285},[{x,1},0]}.
    {move,{literal,<<0,42,0,42>>},{x,0}}.
    {line,[{location,"test_bs.erl",539}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,284},1,[{x,0}],{x,1}}.
    {test,bs_match_string,{f,284},[{x,1},1,{string,<<0>>}]}.
    {test,bs_skip_bits2,
          {f,284},
          [{x,1},
           {integer,3},
           1,
           {field_flags,[{anno,[539,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_match_string,{f,284},[{x,1},12,{string,<<2,160>>}]}.
    {test,bs_skip_bits2,
          {f,284},
          [{x,1},
           {integer,8},
           1,
           {field_flags,[{anno,[539,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_match_string,{f,284},[{x,1},8,{string,<<"*">>}]}.
    {test,bs_test_tail2,{f,284},[{x,1},0]}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,422},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bs_matchable,1}}]}}.
    {move,{literal,{badmatch,<<1,80>>}},{x,1}}.
    {call_last,2,{f,218},0}. % expect_error/2
  {label,284}.
    {badmatch,{x,0}}.
  {label,285}.
    {line,[{location,"test_bs.erl",538}]}.
    {badmatch,{x,0}}.


{function, test_match_case_type, 0, 287}.
  {label,286}.
    {line,[{location,"test_bs.erl",545}]}.
    {func_info,{atom,test_bs},{atom,test_match_case_type},0}.
  {label,287}.
    {allocate,0,0}.
    {move,{literal,[foo,bar]},{x,0}}.
    {line,[{location,"test_bs.erl",546}]}.
    {call,1,{f,291}}. % match_case_type/1
    {'%',{var_info,{x,0},[{type,{t_atom,[bar,foo]}}]}}.
    {test,is_eq_exact,{f,289},[{tr,{x,0},{t_atom,[bar,foo]}},{atom,foo}]}.
    {move,{literal,<<"abc">>},{x,0}}.
    {line,[{location,"test_bs.erl",547}]}.
    {call,1,{f,291}}. % match_case_type/1
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[something_else_entirely]},
                                   none,
                                   {t_integer,{0,255}},
                                   none,none}}]}}.
    {test,is_eq_exact,
          {f,288},
          [{tr,{x,0},
               {t_union,{t_atom,[something_else_entirely]},
                        none,
                        {t_integer,{0,255}},
                        none,none}},
           {integer,97}]}.
    {move,{atom,blahblah},{x,0}}.
    {line,[{location,"test_bs.erl",548}]}.
    {call,1,{f,291}}. % match_case_type/1
    {'%',{var_info,{x,0},[{type,{t_atom,[something_else_entirely]}}]}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,288}.
    {line,[{location,"test_bs.erl",547}]}.
    {badmatch,{x,0}}.
  {label,289}.
    {line,[{location,"test_bs.erl",546}]}.
    {badmatch,{atom,bar}}.


{function, match_case_type, 1, 291}.
  {label,290}.
    {line,[{location,"test_bs.erl",551}]}.
    {func_info,{atom,test_bs},{atom,match_case_type},1}.
  {label,291}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[blahblah]},
                                   {t_cons,{t_atom,[bar,foo]},nil},
                                   none,none,
                                   {t_bitstring,24}}},
                    accepts_match_context]}}.
    {test,bs_start_match3,{f,292},1,[{x,0}],{x,1}}.
    {test,bs_get_integer2,
          {f,293},
          2,
          [{x,1},
           {integer,8},
           1,
           {field_flags,[{anno,[555,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,0}}.
    return.
  {label,292}.
    {test,is_nonempty_list,{f,293},[{x,0}]}.
    {get_hd,{x,0},{x,0}}.
    return.
  {label,293}.
    {move,{atom,something_else_entirely},{x,0}}.
    return.


{function, test_iterate_binary, 0, 295}.
  {label,294}.
    {line,[{location,"test_bs.erl",574}]}.
    {func_info,{atom,test_bs},{atom,test_iterate_binary},0}.
  {label,295}.
    {allocate,0,0}.
    {move,{literal,<<241,131,104,2,100,0,4,99,97,108,108,104,2,104,3,100,0,3,
                     106,111,101,100,0,6,114,111,98,101,114,116,97,0,104,2,
                     100,0,5,104,101,108,108,111,97,1>>},
          {x,0}}.
    {line,[{location,"test_bs.erl",575}]}.
    {call,1,{f,398}}. % id/1
    {move,nil,{x,1}}.
    {call,2,{f,298}}. % traverse/2
    {'%',{var_info,{x,0},[{type,{t_list,{t_integer,{0,255}},nil}}]}}.
    {test,is_eq_exact,
          {f,296},
          [{tr,{x,0},{t_list,{t_integer,{0,255}},nil}},
           {literal,[241,131,104,2,100,0,4,99,97,108,108,104,2,104,3,100,0,3,
                     106,111,101,100,0,6,114,111,98,101,114,116,97,0,104,2,
                     100,0,5,104,101,108,108,111,97,1]}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,296}.
    {badmatch,{x,0}}.


{function, traverse, 2, 298}.
  {label,297}.
    {line,[{location,"test_bs.erl",578}]}.
    {func_info,{atom,test_bs},{atom,traverse},2}.
  {label,298}.
    {'%',{var_info,{x,0},[accepts_match_context]}}.
    {'%',{var_info,{x,1},[{type,{t_list,{t_integer,{0,255}},nil}}]}}.
    {test,bs_start_match3,{f,297},2,[{x,0}],{x,2}}.
    {bs_get_position,{x,2},{x,0},3}.
    {test,bs_test_tail2,{f,299},[{x,2},0]}.
    {move,{x,1},{x,0}}.
    return.
  {label,299}.
    {test,bs_get_integer2,
          {f,300},
          3,
          [{x,2},
           {integer,8},
           1,
           {field_flags,[{anno,[579,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,3}}.
    {test,bs_test_unit,{f,300},[{x,2},8]}.
    {allocate_heap,1,2,4}.
    {move,{x,2},{y,0}}.
    {put_list,{x,3},nil,{x,0}}.
    {swap,{x,0},{x,1}}.
    {line,[{location,"test_bs.erl",579}]}.
    {call_ext,2,{extfunc,erlang,'++',2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {call_last,2,{f,298},1}. % traverse/2
  {label,300}.
    {bs_set_position,{x,2},{x,0}}.
    {bs_get_tail,{x,2},{x,0},3}.
    {jump,{f,297}}.


{function, test_large, 0, 302}.
  {label,301}.
    {line,[{location,"test_bs.erl",581}]}.
    {func_info,{atom,test_bs},{atom,test_large},0}.
  {label,302}.
    {line,[{location,"test_bs.erl",582}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,integer},
                          1,1,nil,
                          {integer,0},
                          {integer,1016},
                          {atom,string},
                          0,8,nil,
                          {string,<<"*">>},
                          {integer,1}]}}.
    {allocate,0,1}.
    {line,[{location,"test_bs.erl",583}]}.
    {call,1,{f,398}}. % id/1
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,1},
                   {list,[{atom,integer},
                          1,1,nil,
                          {integer,0},
                          {integer,1016},
                          {atom,string},
                          0,8,nil,
                          {string,<<"*">>},
                          {integer,1}]}}.
    {test,is_eq_exact,{f,303},[{x,0},{tr,{x,1},{t_bitstring,1}}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,303}.
    {badmatch,{atom,false}}.


{function, test_copy_bits_string, 0, 305}.
  {label,304}.
    {line,[{location,"test_bs.erl",590}]}.
    {func_info,{atom,test_bs},{atom,test_copy_bits_string},0}.
  {label,305}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{integer,42},{x,0}}.
    {line,[{location,"test_bs.erl",591}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,1}}.
    {move,{integer,0},{x,0}}.
    {line,[{location,"test_bs.erl",592}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,0}}.
    {move,{integer,0},{x,0}}.
    {line,[{location,"test_bs.erl",593}]}.
    {call,1,{f,398}}. % id/1
    {line,[{location,"test_bs.erl",594}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,0},
                   {list,[{atom,integer},
                          1,1,
                          {literal,[little]},
                          {y,1},
                          {integer,16},
                          {atom,integer},
                          2,1,nil,
                          {y,0},
                          {integer,7},
                          {atom,string},
                          0,8,nil,
                          {string,<<"Û">>},
                          {integer,1},
                          {atom,integer},
                          6,1,nil,
                          {x,0},
                          {integer,1}]}}.
    {test,is_eq_exact,
          {f,306},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<42,0,1,182>>}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,2}.
    return.
  {label,306}.
    {line,[{location,"test_bs.erl",595}]}.
    {badmatch,{x,0}}.


{function, test_bs_match_string_select, 0, 308}.
  {label,307}.
    {line,[{location,"test_bs.erl",621}]}.
    {func_info,{atom,test_bs},{atom,test_bs_match_string_select},0}.
  {label,308}.
    {allocate,0,0}.
    {move,{literal,<<"ay">>},{x,1}}.
    {move,{literal,<<"ok">>},{x,0}}.
    {line,[{location,"test_bs.erl",625}]}.
    {call,2,{f,402}}. % join/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {bs_start_match4,{atom,no_fail},1,{x,0},{x,0}}.
    {test,bs_get_integer2,
          {f,312},
          1,
          [{x,0},
           {integer,16},
           1,
           {field_flags,[{anno,[626,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,1}}.
    {bs_get_position,{x,0},{x,2},2}.
    {select_val,{x,1},
                {f,312},
                {list,[{integer,24940},{f,311},{integer,28523},{f,309}]}}.
  {label,309}.
    {test,bs_match_string,{f,310},[{x,0},104,{string,<<"_simultaneous">>}]}.
    {test,bs_test_tail2,{f,310},[{x,0},0]}.
    {move,{atom,nok_simultaneous},{x,0}}.
    {jump,{f,313}}.
  {label,310}.
    {bs_set_position,{x,0},{x,2}}.
    {test,bs_test_tail2,{f,312},[{x,0},0]}.
    {move,{atom,nok_ok},{x,0}}.
    {jump,{f,313}}.
  {label,311}.
    {test,bs_match_string,{f,312},[{x,0},24,{string,<<"ive">>}]}.
    {test,bs_test_tail2,{f,312},[{x,0},0]}.
    {move,{atom,nok_alive},{x,0}}.
    {jump,{f,313}}.
  {label,312}.
    {move,{atom,ok},{x,0}}.
  {label,313}.
    {call_last,1,{f,398},0}. % id/1


{function, test_bs_skip_bits2_little, 0, 315}.
  {label,314}.
    {line,[{location,"test_bs.erl",635}]}.
    {func_info,{atom,test_bs},{atom,test_bs_skip_bits2_little},0}.
  {label,315}.
    {allocate,0,0}.
    {move,{literal,<<233,0,0,0,0>>},{x,0}}.
    {line,[{location,"test_bs.erl",636}]}.
    {call,1,{f,398}}. % id/1
    {call,1,{f,393}}. % check_x86_64_jt/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 {t_union,
                     {t_atom,[ok]},
                     none,none,
                     [{{2,{t_atom,[unexpected]}},
                       {t_tuple,2,true,#{1 => {t_atom,[unexpected]}}}}],
                     none}}]}}.
    {test,is_eq_exact,
          {f,316},
          [{tr,{x,0},
               {t_union,{t_atom,[ok]},
                        none,none,
                        [{{2,{t_atom,[unexpected]}},
                          {t_tuple,2,true,#{1 => {t_atom,[unexpected]}}}}],
                        none}},
           {atom,ok}]}.
    {deallocate,0}.
    return.
  {label,316}.
    {badmatch,{x,0}}.


{function, test_bs_variable_size_bitstring, 0, 318}.
  {label,317}.
    {line,[{location,"test_bs.erl",638}]}.
    {func_info,{atom,test_bs},{atom,test_bs_variable_size_bitstring},0}.
  {label,318}.
    {allocate,5,0}.
    {init_yregs,{list,[{y,0},{y,1},{y,2},{y,3},{y,4}]}}.
    {move,{literal,<<1,2>>},{x,0}}.
    {line,[{location,"test_bs.erl",639}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,3}}.
    {move,{literal,<<3,4>>},{x,0}}.
    {line,[{location,"test_bs.erl",640}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,2}}.
    {move,{integer,16},{x,0}}.
    {line,[{location,"test_bs.erl",641}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,1}}.
    {move,{integer,8},{x,0}}.
    {line,[{location,"test_bs.erl",642}]}.
    {call,1,{f,398}}. % id/1
    {line,[{location,"test_bs.erl",643}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,1},
                   {list,[{atom,binary},
                          1,1,nil,
                          {y,3},
                          {y,1},
                          {atom,binary},
                          2,8,nil,
                          {y,2},
                          {atom,all}]}}.
    {test,is_eq_exact,
          {f,332},
          [{tr,{x,1},{t_bitstring,1}},{literal,<<1,2,3,4>>}]}.
    {line,[{scope,[3]},{location,"test_bs.erl",644}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,0},
                   {list,[{atom,binary},
                          1,1,nil,
                          {y,3},
                          {y,1},
                          {atom,binary},
                          2,1,nil,
                          {y,2},
                          {x,0}]}}.
    {test,is_eq_exact,
          {f,331},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<1,2,3>>}]}.
    {move,{atom,all},{x,0}}.
    {line,[{scope,[3]},{location,"test_bs.erl",645}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,0}}.
    {move,{atom,machine},{x,0}}.
    {line,[{scope,[3]},{location,"test_bs.erl",649}]}.
    {call_ext,1,{extfunc,erlang,system_info,1}}.
    {test,is_nonempty_list,{f,330},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {select_val,{x,1},
                {f,330},
                {list,[{integer,65},{f,320},{integer,66},{f,319}]}}.
  {label,319}.
    {test,is_eq_exact,{f,330},[{x,2},{literal,"EAM"}]}.
    {jump,{f,321}}.
  {label,320}.
    {test,is_eq_exact,{f,330},[{x,2},{literal,"TOM"}]}.
    {move,{atom,emu_flavor},{x,0}}.
    {line,[{scope,[3]},{location,"test_bs.erl",653}]}.
    {call_ext,1,{extfunc,erlang,system_info,1}}.
    {select_val,{x,0},{f,329},{list,[{atom,emu},{f,322},{atom,jit},{f,321}]}}.
  {label,321}.
    {move,{atom,no},{x,0}}.
    {jump,{f,323}}.
  {label,322}.
    {move,{atom,ok},{x,0}}.
  {label,323}.
    {'try',{y,4},{f,324}}.
    {line,[{scope,[3]},{location,"test_bs.erl",660}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,1},
                   {list,[{atom,binary},
                          1,1,nil,
                          {y,3},
                          {y,1},
                          {atom,binary},
                          2,1,nil,
                          {y,2},
                          {y,0}]}}.
    {test,is_eq_exact,
          {f,328},
          [{tr,{x,1},{t_bitstring,1}},{literal,<<1,2,3,4>>}]}.
    {try_end,{y,4}}.
    {test,is_eq_exact,{f,327},[{tr,{x,0},{t_atom,[no,ok]}},{atom,ok}]}.
    {jump,{f,325}}.
  {label,324}.
    {try_case,{y,4}}.
    {test,is_eq_exact,
          {f,326},
          [{tr,{x,0},{t_atom,[error,exit,throw]}},{atom,error}]}.
    {test,is_eq_exact,{f,326},[{x,1},{atom,badarg}]}.
  {label,325}.
    {move,{atom,ok},{x,0}}.
    {deallocate,5}.
    return.
  {label,326}.
    {bif,raise,{f,0},[{x,2},{x,1}],{x,0}}.
  {label,327}.
    {line,[{scope,[1]},{location,"test_bs.erl",658}]}.
    {badmatch,{atom,no}}.
  {label,328}.
    {line,[{scope,[2]},{location,"test_bs.erl",660}]}.
    {badmatch,{x,1}}.
  {label,329}.
    {line,[{scope,[3]},{location,"test_bs.erl",653}]}.
    {case_end,{x,0}}.
  {label,330}.
    {line,[{scope,[3]},{location,"test_bs.erl",649}]}.
    {case_end,{x,0}}.
  {label,331}.
    {line,[{scope,[3]},{location,"test_bs.erl",644}]}.
    {badmatch,{x,0}}.
  {label,332}.
    {line,[{scope,[3]},{location,"test_bs.erl",643}]}.
    {badmatch,{x,1}}.


{function, test_float, 0, 334}.
  {label,333}.
    {line,[{location,"test_bs.erl",668}]}.
    {func_info,{atom,test_bs},{atom,test_float},0}.
  {label,334}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{atom,machine},{x,0}}.
    {line,[{location,"test_bs.erl",670}]}.
    {call_ext,1,{extfunc,erlang,system_info,1}}.
    {test,is_nonempty_list,{f,372},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {select_val,{x,1},
                {f,372},
                {list,[{integer,65},{f,336},{integer,66},{f,335}]}}.
  {label,335}.
    {test,is_eq_exact,{f,372},[{x,2},{literal,"EAM"}]}.
    {move,{integer,8},{y,1}}.
    {jump,{f,337}}.
  {label,336}.
    {test,is_eq_exact,{f,372},[{x,2},{literal,"TOM"}]}.
    {move,{atom,avm_floatsize},{x,0}}.
    {line,[{location,"test_bs.erl",672}]}.
    {call_ext,1,{extfunc,erlang,system_info,1}}.
    {move,{x,0},{y,1}}.
  {label,337}.
    {move,{float,3.14},{x,0}}.
    {line,[{location,"test_bs.erl",675}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,0}}.
    {select_val,{y,1},
                {f,371},
                {list,[{integer,4},{f,339},{integer,8},{f,338}]}}.
  {label,338}.
    {line,[{location,"test_bs.erl",678}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,nil,
                          {x,0},
                          {integer,64},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,351},
          [{tr,{x,0},{t_bitstring,1}},
           {literal,<<64,9,30,184,81,235,133,31,3,14>>}]}.
    {line,[{location,"test_bs.erl",679}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,nil,
                          {y,0},
                          {integer,64},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,350},
          [{tr,{x,0},{t_bitstring,1}},
           {literal,<<64,9,30,184,81,235,133,31,3,14>>}]}.
    {line,[{location,"test_bs.erl",680}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,
                          {literal,[little]},
                          {y,0},
                          {integer,64},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,349},
          [{tr,{x,0},{t_bitstring,1}},
           {literal,<<31,133,235,81,184,30,9,64,3,14>>}]}.
    {line,[{location,"test_bs.erl",681}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,
                          {literal,[native]},
                          {y,0},
                          {integer,64},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {bs_start_match4,{atom,no_fail},1,{x,0},{x,1}}.
    {test,bs_skip_bits2,
          {f,348},
          [{x,1},
           {integer,64},
           1,
           {field_flags,[{anno,[681,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_match_string,{f,348},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,348},[{x,1},0]}.
    {line,[{location,"test_bs.erl",682}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,nil,
                          {y,0},
                          {integer,32},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,347},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<64,72,245,195,3,14>>}]}.
    {line,[{location,"test_bs.erl",683}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,
                          {literal,[little]},
                          {y,0},
                          {integer,32},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,346},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<195,245,72,64,3,14>>}]}.
    {init_yregs,{list,[{y,1}]}}.
    {move,{literal,<<64,9,30,184,81,235,133,31,3,14>>},{x,0}}.
    {line,[{location,"test_bs.erl",685}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,345},1,[{x,0}],{x,1}}.
    {test,bs_get_float2,
          {f,345},
          2,
          [{x,1},
           {integer,64},
           1,
           {field_flags,[{anno,[685,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {test,bs_match_string,{f,345},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,345},[{x,1},0]}.
    {test,is_eq_exact,{f,345},[{tr,{x,2},{t_float,any}},{y,0}]}.
    {move,{literal,<<31,133,235,81,184,30,9,64,3,14>>},{x,0}}.
    {line,[{location,"test_bs.erl",686}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,344},1,[{x,0}],{x,1}}.
    {test,bs_get_float2,
          {f,344},
          2,
          [{x,1},
           {integer,64},
           1,
           {field_flags,[{anno,[686,{file,"test_bs.erl"}]},unsigned,little]}],
          {x,2}}.
    {test,bs_match_string,{f,344},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,344},[{x,1},0]}.
    {test,is_eq_exact,
          {f,344},
          [{tr,{x,2},{t_float,any}},{tr,{y,0},{t_float,any}}]}.
    {move,{literal,<<64,72,245,195,3,14>>},{x,0}}.
    {line,[{location,"test_bs.erl",687}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,343},1,[{x,0}],{x,1}}.
    {test,bs_get_float2,
          {f,343},
          2,
          [{x,1},
           {integer,32},
           1,
           {field_flags,[{anno,[687,{file,"test_bs.erl"}]},unsigned,big]}],
          {y,1}}.
    {test,bs_match_string,{f,343},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,343},[{x,1},0]}.
    {move,{literal,<<195,245,72,64,3,14>>},{x,0}}.
    {line,[{location,"test_bs.erl",688}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,342},1,[{x,0}],{x,1}}.
    {test,bs_get_float2,
          {f,342},
          2,
          [{x,1},
           {integer,32},
           1,
           {field_flags,[{anno,[688,{file,"test_bs.erl"}]},unsigned,little]}],
          {x,2}}.
    {test,bs_match_string,{f,342},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,342},[{x,1},0]}.
    {test,is_eq_exact,
          {f,342},
          [{tr,{x,2},{t_float,any}},{tr,{y,1},{t_float,any}}]}.
    {fmove,{y,1},{fr,0}}.
    {fmove,{y,0},{fr,1}}.
    {line,[{location,"test_bs.erl",689}]}.
    {bif,fsub,{f,0},[{fr,0},{fr,1}],{fr,0}}.
    {test_heap,{alloc,[{words,0},{floats,1},{funs,0}]},0}.
    {fmove,{fr,0},{x,0}}.
    {gc_bif,abs,{f,0},1,[{tr,{x,0},{t_float,any}}],{x,0}}.
    {test,is_lt,{f,341},[{tr,{x,0},{t_float,any}},{float,0.0001}]}.
    {jump,{f,340}}.
  {label,339}.
    {line,[{location,"test_bs.erl",691}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,nil,
                          {x,0},
                          {integer,64},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,370},
          [{tr,{x,0},{t_bitstring,1}},
           {literal,<<64,9,30,184,96,0,0,0,3,14>>}]}.
    {line,[{location,"test_bs.erl",692}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,nil,
                          {y,0},
                          {integer,64},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,369},
          [{tr,{x,0},{t_bitstring,1}},
           {literal,<<64,9,30,184,96,0,0,0,3,14>>}]}.
    {line,[{location,"test_bs.erl",693}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,
                          {literal,[little]},
                          {y,0},
                          {integer,64},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,368},
          [{tr,{x,0},{t_bitstring,1}},
           {literal,<<0,0,0,96,184,30,9,64,3,14>>}]}.
    {line,[{location,"test_bs.erl",694}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,
                          {literal,[native]},
                          {y,0},
                          {integer,64},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {bs_start_match4,{atom,no_fail},1,{x,0},{x,1}}.
    {test,bs_skip_bits2,
          {f,367},
          [{x,1},
           {integer,64},
           1,
           {field_flags,[{anno,[694,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_match_string,{f,367},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,367},[{x,1},0]}.
    {line,[{location,"test_bs.erl",695}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,nil,
                          {y,0},
                          {integer,32},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,366},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<64,72,245,195,3,14>>}]}.
    {line,[{location,"test_bs.erl",696}]}.
    {bs_create_bin,{f,0},
                   0,0,1,
                   {x,0},
                   {list,[{atom,float},
                          1,1,
                          {literal,[little]},
                          {y,0},
                          {integer,32},
                          {atom,string},
                          0,8,nil,
                          {string,<<3,14>>},
                          {integer,2}]}}.
    {test,is_eq_exact,
          {f,365},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<195,245,72,64,3,14>>}]}.
    {init_yregs,{list,[{y,1}]}}.
    {move,{literal,<<64,9,30,184,96,0,0,0,3,14>>},{x,0}}.
    {line,[{location,"test_bs.erl",698}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,364},1,[{x,0}],{x,1}}.
    {test,bs_get_float2,
          {f,364},
          2,
          [{x,1},
           {integer,64},
           1,
           {field_flags,[{anno,[698,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {test,bs_match_string,{f,364},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,364},[{x,1},0]}.
    {test,is_eq_exact,{f,364},[{tr,{x,2},{t_float,any}},{y,0}]}.
    {move,{literal,<<0,0,0,96,184,30,9,64,3,14>>},{x,0}}.
    {line,[{location,"test_bs.erl",699}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,363},1,[{x,0}],{x,1}}.
    {test,bs_get_float2,
          {f,363},
          2,
          [{x,1},
           {integer,64},
           1,
           {field_flags,[{anno,[699,{file,"test_bs.erl"}]},unsigned,little]}],
          {x,2}}.
    {test,bs_match_string,{f,363},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,363},[{x,1},0]}.
    {test,is_eq_exact,
          {f,363},
          [{tr,{x,2},{t_float,any}},{tr,{y,0},{t_float,any}}]}.
    {move,{literal,<<64,72,245,195,3,14>>},{x,0}}.
    {line,[{location,"test_bs.erl",700}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,362},1,[{x,0}],{x,1}}.
    {test,bs_get_float2,
          {f,362},
          2,
          [{x,1},
           {integer,32},
           1,
           {field_flags,[{anno,[700,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {test,bs_match_string,{f,362},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,362},[{x,1},0]}.
    {test,is_eq_exact,
          {f,362},
          [{tr,{x,2},{t_float,any}},{tr,{y,0},{t_float,any}}]}.
    {move,{literal,<<195,245,72,64,3,14>>},{x,0}}.
    {line,[{location,"test_bs.erl",701}]}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,361},1,[{x,0}],{x,1}}.
    {test,bs_get_float2,
          {f,361},
          2,
          [{x,1},
           {integer,32},
           1,
           {field_flags,[{anno,[701,{file,"test_bs.erl"}]},unsigned,little]}],
          {x,2}}.
    {test,bs_match_string,{f,361},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,361},[{x,1},0]}.
    {test,is_eq_exact,
          {f,361},
          [{tr,{x,2},{t_float,any}},{tr,{y,0},{t_float,any}}]}.
  {label,340}.
    {init_yregs,{list,[{y,0}]}}.
    {move,{integer,2},{x,0}}.
    {line,[{location,"test_bs.erl",705}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,1}}.
    {move,{integer,-2},{x,0}}.
    {line,[{location,"test_bs.erl",706}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,0}}.
    {move,{integer,32},{x,0}}.
    {line,[{location,"test_bs.erl",707}]}.
    {call,1,{f,398}}. % id/1
    {line,[{location,"test_bs.erl",708}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,1},
                   {list,[{atom,float},1,1,nil,{y,1},{integer,64}]}}.
    {test,is_eq_exact,
          {f,360},
          [{tr,{x,1},{t_bitstring,1}},{literal,<<64,0,0,0,0,0,0,0>>}]}.
    {line,[{location,"test_bs.erl",709}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,1},
                   {list,[{atom,float},1,1,nil,{y,0},{integer,64}]}}.
    {test,is_eq_exact,
          {f,359},
          [{tr,{x,1},{t_bitstring,1}},{literal,<<192,0,0,0,0,0,0,0>>}]}.
    {line,[{location,"test_bs.erl",710}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,0},
                   {list,[{atom,float},1,1,nil,{x,0},{integer,32}]}}.
    {test,is_eq_exact,
          {f,358},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<66,0,0,0>>}]}.
    {trim,2,0}.
    {move,{float,3.14},{x,0}}.
    {line,[{location,"test_bs.erl",712}]}.
    {call,1,{f,398}}. % id/1
    {line,[{location,"test_bs.erl",713}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,1},
                   {list,[{atom,float},1,1,nil,{x,0},{integer,16}]}}.
    {test,is_eq_exact,{f,357},[{tr,{x,1},{t_bitstring,1}},{literal,<<"BH">>}]}.
    {line,[{location,"test_bs.erl",714}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,1},
                   {list,[{atom,float},1,1,nil,{x,0},{integer,16}]}}.
    {test,is_eq_exact,{f,356},[{tr,{x,1},{t_bitstring,1}},{literal,<<"BH">>}]}.
    {line,[{location,"test_bs.erl",715}]}.
    {bs_create_bin,{f,0},
                   0,1,1,
                   {x,1},
                   {list,[{atom,float},
                          1,1,
                          {literal,[little]},
                          {x,0},
                          {integer,16}]}}.
    {test,is_eq_exact,{f,355},[{tr,{x,1},{t_bitstring,1}},{literal,<<"HB">>}]}.
    {move,{literal,<<66,72,3,14>>},{x,1}}.
    {bs_start_match4,{atom,no_fail},1,{x,1},{x,1}}.
    {test,bs_get_float2,
          {f,354},
          2,
          [{x,1},
           {integer,16},
           1,
           {field_flags,[{anno,[716,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {test,bs_match_string,{f,354},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,354},[{x,1},0]}.
    {move,{literal,<<72,66,3,14>>},{x,1}}.
    {bs_start_match4,{atom,no_fail},3,{x,1},{x,1}}.
    {test,bs_get_float2,
          {f,353},
          3,
          [{x,1},
           {integer,16},
           1,
           {field_flags,[{anno,[717,{file,"test_bs.erl"}]},unsigned,little]}],
          {x,3}}.
    {test,bs_match_string,{f,353},[{x,1},16,{string,<<3,14>>}]}.
    {test,bs_test_tail2,{f,353},[{x,1},0]}.
    {test,is_eq_exact,
          {f,353},
          [{tr,{x,3},{t_float,any}},{tr,{x,2},{t_float,any}}]}.
    {fmove,{x,2},{fr,0}}.
    {line,[{location,"test_bs.erl",718}]}.
    {fconv,{x,0},{fr,1}}.
    {bif,fsub,{f,0},[{fr,0},{fr,1}],{fr,0}}.
    {test_heap,{alloc,[{words,0},{floats,1},{funs,0}]},0}.
    {fmove,{fr,0},{x,0}}.
    {gc_bif,abs,{f,0},1,[{tr,{x,0},number}],{x,0}}.
    {test,is_lt,{f,352},[{tr,{x,0},number},{float,0.001}]}.
    {line,[{location,"test_bs.erl",720}]}.
    {call,0,{f,378}}. % test_integer_outside_float_limits/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"test_bs.erl",721}]}.
    {call,0,{f,374}}. % test_create_with_invalid_float_value/0
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {call_last,0,{f,376},0}. % test_create_with_invalid_float_size/0
  {label,341}.
    {line,[{location,"test_bs.erl",689}]}.
    {badmatch,{atom,false}}.
  {label,342}.
    {line,[{location,"test_bs.erl",688}]}.
    {badmatch,{x,0}}.
  {label,343}.
    {line,[{location,"test_bs.erl",687}]}.
    {badmatch,{x,0}}.
  {label,344}.
    {line,[{location,"test_bs.erl",686}]}.
    {badmatch,{x,0}}.
  {label,345}.
    {line,[{location,"test_bs.erl",685}]}.
    {badmatch,{x,0}}.
  {label,346}.
    {line,[{location,"test_bs.erl",683}]}.
    {badmatch,{x,0}}.
  {label,347}.
    {line,[{location,"test_bs.erl",682}]}.
    {badmatch,{x,0}}.
  {label,348}.
    {line,[{location,"test_bs.erl",681}]}.
    {badmatch,{x,0}}.
  {label,349}.
    {line,[{location,"test_bs.erl",680}]}.
    {badmatch,{x,0}}.
  {label,350}.
    {line,[{location,"test_bs.erl",679}]}.
    {badmatch,{x,0}}.
  {label,351}.
    {line,[{location,"test_bs.erl",678}]}.
    {badmatch,{x,0}}.
  {label,352}.
    {line,[{location,"test_bs.erl",718}]}.
    {badmatch,{atom,false}}.
  {label,353}.
    {line,[{location,"test_bs.erl",717}]}.
    {badmatch,{literal,<<72,66,3,14>>}}.
  {label,354}.
    {line,[{location,"test_bs.erl",716}]}.
    {badmatch,{literal,<<66,72,3,14>>}}.
  {label,355}.
    {line,[{location,"test_bs.erl",715}]}.
    {badmatch,{x,1}}.
  {label,356}.
    {line,[{location,"test_bs.erl",714}]}.
    {badmatch,{x,1}}.
  {label,357}.
    {line,[{location,"test_bs.erl",713}]}.
    {badmatch,{x,1}}.
  {label,358}.
    {line,[{location,"test_bs.erl",710}]}.
    {badmatch,{x,0}}.
  {label,359}.
    {line,[{location,"test_bs.erl",709}]}.
    {badmatch,{x,1}}.
  {label,360}.
    {line,[{location,"test_bs.erl",708}]}.
    {badmatch,{x,1}}.
  {label,361}.
    {line,[{location,"test_bs.erl",701}]}.
    {badmatch,{x,0}}.
  {label,362}.
    {line,[{location,"test_bs.erl",700}]}.
    {badmatch,{x,0}}.
  {label,363}.
    {line,[{location,"test_bs.erl",699}]}.
    {badmatch,{x,0}}.
  {label,364}.
    {line,[{location,"test_bs.erl",698}]}.
    {badmatch,{x,0}}.
  {label,365}.
    {line,[{location,"test_bs.erl",696}]}.
    {badmatch,{x,0}}.
  {label,366}.
    {line,[{location,"test_bs.erl",695}]}.
    {badmatch,{x,0}}.
  {label,367}.
    {line,[{location,"test_bs.erl",694}]}.
    {badmatch,{x,0}}.
  {label,368}.
    {line,[{location,"test_bs.erl",693}]}.
    {badmatch,{x,0}}.
  {label,369}.
    {line,[{location,"test_bs.erl",692}]}.
    {badmatch,{x,0}}.
  {label,370}.
    {line,[{location,"test_bs.erl",691}]}.
    {badmatch,{x,0}}.
  {label,371}.
    {line,[{location,"test_bs.erl",676}]}.
    {case_end,{y,1}}.
  {label,372}.
    {line,[{location,"test_bs.erl",670}]}.
    {case_end,{x,0}}.


{function, test_create_with_invalid_float_value, 0, 374}.
  {label,373}.
    {line,[{location,"test_bs.erl",725}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_invalid_float_value},0}.
  {label,374}.
    {allocate_heap,0,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,420},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {line,[{location,"test_bs.erl",726}]}.
    {call,2,{f,218}}. % expect_error/2
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,418},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {line,[{location,"test_bs.erl",727}]}.
    {call,2,{f,218}}. % expect_error/2
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,416},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {call_last,2,{f,218},0}. % expect_error/2


{function, test_create_with_invalid_float_size, 0, 376}.
  {label,375}.
    {line,[{location,"test_bs.erl",731}]}.
    {func_info,{atom,test_bs},{atom,test_create_with_invalid_float_size},0}.
  {label,376}.
    {allocate_heap,0,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,414},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {line,[{location,"test_bs.erl",733}]}.
    {call,2,{f,218}}. % expect_error/2
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,412},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {line,[{location,"test_bs.erl",734}]}.
    {call,2,{f,218}}. % expect_error/2
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {test_heap,{alloc,[{words,0},{floats,0},{funs,1}]},0}.
    {make_fun3,{f,410},0,0,{x,0},{list,[]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_bitstring,1}}]}}.
    {move,{atom,badarg},{x,1}}.
    {call_last,2,{f,218},0}. % expect_error/2


{function, test_integer_outside_float_limits, 0, 378}.
  {label,377}.
    {line,[{location,"test_bs.erl",738}]}.
    {func_info,{atom,test_bs},{atom,test_integer_outside_float_limits},0}.
  {label,378}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{integer,115792089237316195423570985008687907853269984665640564039457584007913129639935},
          {x,0}}.
    {line,[{location,"test_bs.erl",739}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,1}}.
    {move,{atom,machine},{x,0}}.
    {line,[{location,"test_bs.erl",742}]}.
    {call_ext,1,{extfunc,erlang,system_info,1}}.
    {test,is_nonempty_list,{f,389},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {select_val,{x,1},
                {f,389},
                {list,[{integer,65},{f,380},{integer,66},{f,379}]}}.
  {label,379}.
    {test,is_eq_exact,{f,389},[{x,2},{literal,"EAM"}]}.
    {jump,{f,381}}.
  {label,380}.
    {test,is_eq_exact,{f,389},[{x,2},{literal,"TOM"}]}.
    {move,{atom,avm_floatsize},{x,0}}.
    {line,[{location,"test_bs.erl",744}]}.
    {call_ext,1,{extfunc,erlang,system_info,1}}.
    {test_heap,{alloc,[{words,1},{floats,0},{funs,1}]},1}.
    {make_fun3,{f,408},0,0,{x,1},{list,[{y,1}]}}.
    {'%',{var_info,{x,1},[{fun_type,{t_bitstring,1}}]}}.
    {select_val,{x,0},
                {f,388},
                {list,[{integer,4},{f,382},{integer,8},{f,381}]}}.
  {label,381}.
    {move,{y,1},{x,0}}.
    {line,[{location,"test_bs.erl",753}]}.
    {call,1,{f,408}}. % '-test_integer_outside_float_limits/0-fun-0-'/1
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,387},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<79,240,0,0,0,0,0,0>>}]}.
    {move,{integer,32},{x,0}}.
    {line,[{location,"test_bs.erl",758}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call,2,{f,391}}. % create_float_binary/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,386},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<127,128,0,0>>}]}.
    {line,[{location,"test_bs.erl",759}]}.
    {gc_bif,'-',{f,0},0,[{y,1}],{y,0}}.
    {move,{integer,32},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,1}.
    {call,2,{f,391}}. % create_float_binary/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,385},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<255,128,0,0>>}]}.
    {move,{integer,16},{x,0}}.
    {line,[{location,"test_bs.erl",761}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {call,2,{f,391}}. % create_float_binary/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,384},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<124,0>>}]}.
    {line,[{location,"test_bs.erl",762}]}.
    {gc_bif,'-',{f,0},0,[{tr,{y,0},number}],{y,0}}.
    {move,{integer,16},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,0}.
    {call,2,{f,391}}. % create_float_binary/2
    {'%',{var_info,{x,0},[{type,{t_bitstring,1}}]}}.
    {test,is_eq_exact,
          {f,383},
          [{tr,{x,0},{t_bitstring,1}},{literal,<<252,0>>}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,382}.
    {move,{x,1},{x,0}}.
    {move,{atom,badarg},{x,1}}.
    {call_last,2,{f,218},2}. % expect_error/2
  {label,383}.
    {badmatch,{x,0}}.
  {label,384}.
    {line,[{location,"test_bs.erl",761}]}.
    {badmatch,{x,0}}.
  {label,385}.
    {line,[{location,"test_bs.erl",759}]}.
    {badmatch,{x,0}}.
  {label,386}.
    {line,[{location,"test_bs.erl",758}]}.
    {badmatch,{x,0}}.
  {label,387}.
    {line,[{location,"test_bs.erl",753}]}.
    {badmatch,{x,0}}.
  {label,388}.
    {line,[{location,"test_bs.erl",749}]}.
    {case_end,{x,0}}.
  {label,389}.
    {line,[{location,"test_bs.erl",742}]}.
    {case_end,{x,0}}.


{function, create_float_binary, 2, 391}.
  {label,390}.
    {line,[{location,"test_bs.erl",767}]}.
    {func_info,{atom,test_bs},{atom,create_float_binary},2}.
  {label,391}.
    {line,[{location,"test_bs.erl",768}]}.
    {bs_create_bin,{f,0},
                   0,2,1,
                   {x,0},
                   {list,[{atom,float},1,1,nil,{x,0},{x,1}]}}.
    return.


{function, check_x86_64_jt, 1, 393}.
  {label,392}.
    {line,[{location,"test_bs.erl",770}]}.
    {func_info,{atom,test_bs},{atom,check_x86_64_jt},1}.
  {label,393}.
    {'%',{var_info,{x,0},[accepts_match_context]}}.
    {test,bs_start_match3,{f,396},1,[{x,0}],{x,1}}.
    {bs_get_position,{x,1},{x,0},2}.
    {test,bs_match_string,{f,394},[{x,1},8,{string,<<"é">>}]}.
    {test,bs_skip_bits2,
          {f,395},
          [{x,1},
           {integer,32},
           1,
           {field_flags,[{anno,[771,{file,"test_bs.erl"}]},unsigned,little]}]}.
    {test,bs_test_unit,{f,395},[{x,1},8]}.
    {move,{x,1},{x,0}}.
    {call_only,1,{f,393}}. % check_x86_64_jt/1
  {label,394}.
    {test,bs_test_tail2,{f,395},[{x,1},0]}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,395}.
    {bs_set_position,{x,1},{x,0}}.
    {bs_get_tail,{x,1},{x,0},2}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,unexpected},{x,0}]}}.
    return.
  {label,396}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,unexpected},{x,0}]}}.
    return.


{function, id, 1, 398}.
  {label,397}.
    {line,[{location,"test_bs.erl",774}]}.
    {func_info,{atom,test_bs},{atom,id},1}.
  {label,398}.
    {call_ext_only,1,{extfunc,test_bs,ext_id,1}}.


{function, ext_id, 1, 400}.
  {label,399}.
    {line,[{location,"test_bs.erl",776}]}.
    {func_info,{atom,test_bs},{atom,ext_id},1}.
  {label,400}.
    return.


{function, join, 2, 402}.
  {label,401}.
    {line,[{location,"test_bs.erl",778}]}.
    {func_info,{atom,test_bs},{atom,join},2}.
  {label,402}.
    {line,[{location,"test_bs.erl",779}]}.
    {bs_create_bin,{f,0},
                   0,2,8,
                   {x,0},
                   {list,[{atom,append},
                          1,8,nil,
                          {x,0},
                          {atom,all},
                          {atom,binary},
                          2,8,nil,
                          {x,1},
                          {atom,all}]}}.
    return.


{function, module_info, 0, 404}.
  {label,403}.
    {line,[]}.
    {func_info,{atom,test_bs},{atom,module_info},0}.
  {label,404}.
    {move,{atom,test_bs},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 406}.
  {label,405}.
    {line,[]}.
    {func_info,{atom,test_bs},{atom,module_info},1}.
  {label,406}.
    {move,{x,0},{x,1}}.
    {move,{atom,test_bs},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-test_integer_outside_float_limits/0-fun-0-', 1, 408}.
  {label,407}.
    {line,[{location,"test_bs.erl",747}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_integer_outside_float_limits/0-fun-0-'},
               1}.
  {label,408}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {move,{integer,64},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {call_last,2,{f,391},1}. % create_float_binary/2


{function, '-test_create_with_invalid_float_size/0-fun-2-', 0, 410}.
  {label,409}.
    {line,[{location,"test_bs.erl",735}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_float_size/0-fun-2-'},
               0}.
  {label,410}.
    {allocate,0,0}.
    {move,{atom,foo},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{float,3.14},{x,0}}.
    {call_last,2,{f,391},0}. % create_float_binary/2


{function, '-test_create_with_invalid_float_size/0-fun-1-', 0, 412}.
  {label,411}.
    {line,[{location,"test_bs.erl",734}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_float_size/0-fun-1-'},
               0}.
  {label,412}.
    {allocate,0,0}.
    {move,{integer,128},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{float,3.14},{x,0}}.
    {call_last,2,{f,391},0}. % create_float_binary/2


{function, '-test_create_with_invalid_float_size/0-fun-0-', 0, 414}.
  {label,413}.
    {line,[{location,"test_bs.erl",733}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_float_size/0-fun-0-'},
               0}.
  {label,414}.
    {allocate,0,0}.
    {move,{integer,8},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{float,3.14},{x,0}}.
    {call_last,2,{f,391},0}. % create_float_binary/2


{function, '-test_create_with_invalid_float_value/0-fun-2-', 0, 416}.
  {label,415}.
    {line,[{location,"test_bs.erl",728}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_float_value/0-fun-2-'},
               0}.
  {label,416}.
    {allocate,0,0}.
    {move,{integer,64},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{literal,<<"binary">>},{x,0}}.
    {call_last,2,{f,391},0}. % create_float_binary/2


{function, '-test_create_with_invalid_float_value/0-fun-1-', 0, 418}.
  {label,417}.
    {line,[{location,"test_bs.erl",727}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_float_value/0-fun-1-'},
               0}.
  {label,418}.
    {allocate,0,0}.
    {move,{integer,32},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{literal,[1,2,3]},{x,0}}.
    {call_last,2,{f,391},0}. % create_float_binary/2


{function, '-test_create_with_invalid_float_value/0-fun-0-', 0, 420}.
  {label,419}.
    {line,[{location,"test_bs.erl",726}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_float_value/0-fun-0-'},
               0}.
  {label,420}.
    {allocate,0,0}.
    {move,{integer,64},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{atom,foo},{x,0}}.
    {call_last,2,{f,391},0}. % create_float_binary/2


{function, '-test_bs_match_string_unaligned/0-fun-0-', 0, 422}.
  {label,421}.
    {line,[{location,"test_bs.erl",541}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_bs_match_string_unaligned/0-fun-0-'},
               0}.
  {label,422}.
    {allocate,0,0}.
    {move,{literal,<<1,80>>},{x,0}}.
    {call,1,{f,398}}. % id/1
    {test,bs_start_match3,{f,423},1,[{x,0}],{x,1}}.
    {test,bs_match_string,{f,423},[{x,1},1,{string,<<0>>}]}.
    {test,bs_skip_bits2,
          {f,423},
          [{x,1},
           {integer,4},
           1,
           {field_flags,[{anno,[541,{file,"test_bs.erl"}]},unsigned,big]}]}.
    {test,bs_match_string,{f,423},[{x,1},19,{string,<<2,160,0>>}]}.
    {test,bs_test_tail2,{f,423},[{x,1},0]}.
    {deallocate,0}.
    return.
  {label,423}.
    {badmatch,{x,0}}.


{function, '-test_bs_private_append/1-lbc$^0/2-0-', 2, 425}.
  {label,424}.
    {line,[{location,"test_bs.erl",463}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_bs_private_append/1-lbc$^0/2-0-'},
               2}.
  {label,425}.
    {'%',{var_info,{x,0},[{type,{t_bs_matchable,8}},accepts_match_context]}}.
    {bs_start_match4,{atom,no_fail},2,{x,0},{x,0}}.
    {test,bs_get_integer2,
          {f,426},
          2,
          [{x,0},
           {integer,8},
           1,
           {field_flags,[{anno,[463,{file,"test_bs.erl"}]},unsigned,big]}],
          {x,2}}.
    {bs_create_bin,{f,0},
                   0,3,1,
                   {x,1},
                   {list,[{atom,private_append},
                          1,1,nil,
                          {x,1},
                          {atom,all},
                          {atom,integer},
                          2,1,nil,
                          {tr,{x,2},{t_integer,{0,255}}},
                          {integer,8}]}}.
    {call_only,2,{f,425}}. % '-test_bs_private_append/1-lbc$^0/2-0-'/2
  {label,426}.
    {move,{x,1},{x,0}}.
    return.


{function, '-atom_unsupported/1-fun-0-', 1, 428}.
  {label,427}.
    {line,[{location,"test_bs.erl",434}]}.
    {func_info,{atom,test_bs},{atom,'-atom_unsupported/1-fun-0-'},1}.
  {label,428}.
    {bif,'=:=',{f,0},[{x,0},{atom,unsupported}],{x,0}}.
    return.


{function, '-expect_error/2-fun-0-', 3, 430}.
  {label,429}.
    {line,[{location,"test_bs.erl",416}]}.
    {func_info,{atom,test_bs},{atom,'-expect_error/2-fun-0-'},3}.
  {label,430}.
    {'%',
        {var_info,
            {x,2},
            [{type,
                 {t_union,
                     {t_atom,[badarg]},
                     none,none,
                     [{{2,{t_atom,[badmatch]}},
                       {t_tuple,2,true,
                           #{1 => {t_atom,[badmatch]},
                             2 => {t_bitstring,16}}}}],
                     none}}]}}.
    {test,is_eq_exact,{f,431},[{x,0},{atom,error}]}.
    {bif,'=:=',
         {f,0},
         [{x,1},
          {tr,{x,2},
              {t_union,{t_atom,[badarg]},
                       none,none,
                       [{{2,{t_atom,[badmatch]}},
                         {t_tuple,2,true,
                                  #{1 => {t_atom,[badmatch]},
                                    2 => {t_bitstring,16}}}}],
                       none}}],
         {x,0}}.
    return.
  {label,431}.
    {move,{atom,false},{x,0}}.
    return.


{function, '-test_get_with_unaligned_binary/0-fun-0-', 1, 433}.
  {label,432}.
    {line,[{location,"test_bs.erl",357}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_get_with_unaligned_binary/0-fun-0-'},
               1}.
  {label,433}.
    {bif,'=:=',{f,0},[{x,0},{atom,badarg}],{x,0}}.
    return.


{function, '-test_get_with_unaligned_binary/0-fun-1-', 0, 435}.
  {label,434}.
    {line,[{location,"test_bs.erl",357}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_get_with_unaligned_binary/0-fun-1-'},
               0}.
  {label,435}.
    {allocate,1,0}.
    {init_yregs,{list,[{y,0}]}}.
    {move,{integer,4},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{literal,<<1,2,3,4>>},{x,0}}.
    {call_last,3,{f,212},1}. % get_int_then_binary/3


{function, '-test_get_with_unsupported_int_unit/0-fun-0-', 1, 437}.
  {label,436}.
    {line,[{location,"test_bs.erl",277}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_get_with_unsupported_int_unit/0-fun-0-'},
               1}.
  {label,437}.
    {move,{atom,true},{x,0}}.
    return.


{function, '-test_get_with_unsupported_int_unit/0-fun-1-', 0, 439}.
  {label,438}.
    {line,[{location,"test_bs.erl",273}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_get_with_unsupported_int_unit/0-fun-1-'},
               0}.
  {label,439}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{integer,255},{x,0}}.
    {line,[{location,"test_bs.erl",274}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,1}}.
    {move,{literal,<<"ÿÿÿÿ">>},{x,0}}.
    {line,[{location,"test_bs.erl",275}]}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{y,0}}.
    {move,{integer,8},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,1}.
    {call,2,{f,192}}. % get_integer_big_unsigned_unit_3/2
    {'%',{var_info,{x,0},[{type,{t_integer,any}}]}}.
    {test,is_eq_exact,{f,440},[{tr,{x,0},{t_integer,any}},{y,0}]}.
    {deallocate,1}.
    return.
  {label,440}.
    {badmatch,{x,0}}.


{function, '-test_create_with_unsupported_binary_unit/0-fun-0-', 0, 442}.
  {label,441}.
    {line,[{location,"test_bs.erl",267}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_unsupported_binary_unit/0-fun-0-'},
               0}.
  {label,442}.
    {allocate,0,0}.
    {move,{integer,3},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{literal,<<"foo">>},{x,0}}.
    {call_last,2,{f,187},0}. % create_binary_binary_unit_3/2


{function, '-test_create_with_binary_size_out_of_range/0-fun-0-', 0, 444}.
  {label,443}.
    {line,[{location,"test_bs.erl",264}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_binary_size_out_of_range/0-fun-0-'},
               0}.
  {label,444}.
    {allocate,0,0}.
    {move,{integer,4},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{literal,<<"foo">>},{x,0}}.
    {call_last,2,{f,185},0}. % create_binary_binary/2


{function, '-test_create_with_invalid_binary_size/0-fun-0-', 0, 446}.
  {label,445}.
    {line,[{location,"test_bs.erl",261}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_binary_size/0-fun-0-'},
               0}.
  {label,446}.
    {allocate,0,0}.
    {move,{atom,bar},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{literal,<<"foo">>},{x,0}}.
    {call_last,2,{f,185},0}. % create_binary_binary/2


{function, '-test_create_with_invalid_binary_value/0-fun-0-', 0, 448}.
  {label,447}.
    {line,[{location,"test_bs.erl",258}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_binary_value/0-fun-0-'},
               0}.
  {label,448}.
    {allocate,0,0}.
    {move,{integer,32},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{atom,foo},{x,0}}.
    {call_last,2,{f,185},0}. % create_binary_binary/2


{function, '-test_create_with_unsupported_unaligned_int_size/0-fun-0-', 0, 450}.
  {label,449}.
    {line,[{location,"test_bs.erl",228}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_unsupported_unaligned_int_size/0-fun-0-'},
               0}.
  {label,450}.
    {allocate,0,0}.
    {move,{integer,28},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,65535},{x,0}}.
    {call_last,2,{f,177},0}. % create_int_binary/2


{function, '-test_create_with_invalid_int_size/0-fun-0-', 0, 452}.
  {label,451}.
    {line,[{location,"test_bs.erl",216}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_int_size/0-fun-0-'},
               0}.
  {label,452}.
    {allocate,0,0}.
    {move,{atom,bar},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{integer,15},{x,0}}.
    {call_last,2,{f,177},0}. % create_int_binary/2


{function, '-test_create_with_invalid_int_value/0-fun-0-', 0, 454}.
  {label,453}.
    {line,[{location,"test_bs.erl",213}]}.
    {func_info,{atom,test_bs},
               {atom,'-test_create_with_invalid_int_value/0-fun-0-'},
               0}.
  {label,454}.
    {allocate,0,0}.
    {move,{integer,32},{x,0}}.
    {call,1,{f,398}}. % id/1
    {move,{x,0},{x,1}}.
    {move,{atom,foo},{x,0}}.
    {call_last,2,{f,177},0}. % create_int_binary/2


{function, '-start_3a3_e/0-fun-1-', 0, 456}.
  {label,455}.
    {line,[{location,"test_bs.erl",157}]}.
    {func_info,{atom,test_bs},{atom,'-start_3a3_e/0-fun-1-'},0}.
  {label,456}.
    {move,{literal,<<"foobar">>},{x,1}}.
    {move,{integer,1},{x,0}}.
    {call_only,2,{f,280}}. % skip_bits/2


{function, '-start_3a3_d/0-fun-0-', 0, 458}.
  {label,457}.
    {line,[{location,"test_bs.erl",153}]}.
    {func_info,{atom,test_bs},{atom,'-start_3a3_d/0-fun-0-'},0}.
  {label,458}.
    {move,{literal,<<"foobar">>},{x,1}}.
    {move,{integer,128},{x,0}}.
    {call_only,2,{f,280}}. % skip_bits/2
