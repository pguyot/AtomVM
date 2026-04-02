%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%%-----------------------------------------------------------------------------
%% @doc BEAM compatibility shim for AtomVM-specific functions.
%%
%% Provides a minimal implementation of AtomVM functions that are used
%% by the distribution modules (`dist_util', etc.) but are not available
%% on BEAM.
%% @end
%%-----------------------------------------------------------------------------
-module(atomvm).

-export([get_creation/0]).

%% @doc Get a creation number for the distribution handshake.
%%
%% On AtomVM this returns the VM's creation number. On BEAM, we
%% generate a random creation since serial_dist does not use EPMD.
-spec get_creation() -> non_neg_integer().
get_creation() ->
    erlang:unique_integer([positive]) rem 16#100000000.
