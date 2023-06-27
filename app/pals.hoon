::  pals: manual peer discovery
::
::    acts as a "friendlist" of sorts, letting one add arbitrary ships to
::    arbitrary lists. upon doing so, the other party is informed of this.
::    this lets the app expose "friend requests" and mutuals, in addition
::    to user-defined sets of friends.
::
::    intended use case is as simple, bare-bones peer discovery and
::    permissioning for truly peer-to-peer applications, in place of
::    (or as supplement to) group-based peer discovery.
::    for example, a game wanting to stay abreast of high scores,
::    or filesharing service giving selective access.
::
::    "leeches" are ships who added us.
::    "targets" are ships we have added.
::    "mutuals" is the intersection of "leeches" and "targets".
::
::      reading
::    external applications likely want to read from this via scries or
::    watches, both of which are outlined below.
::    finding interaction targets or mutuals to poke or subscribe to, using
::    mutual status as permission check, etc.
::    to scry data out of this app, please use /lib/pals.
::    one might be tempted to use list names for namespacing (ie %yourapp
::    would only retrieve targets from the ~.yourapp list), but beware that
::    this overlaps with user-facing organizational purposes. if lists feel
::    opaque or inaccessible, it's to discourage this. but the right balance
::    might not have been found yet...
::
::      writing
::    poke this app with a $command.
::    %meet adds a ship. it is also added to any list names specified.
::    %part removes a ship from either all or the specified lists.
::    the ~. list name is reserved and cannot be added to.
::    managing pals without an interface that lets users control that behavior
::    is bad manners. managing pals without informing the user is evil.
::
::      scry endpoints (all %noun marks)
::    y  /                       arch        [%leeches %targets %mutuals ~]
::    y  /[status]               arch        non-empty lists listing
::
::    x  /leeches                (set ship)  foreign one-sided friendships
::    x  /targets(/[list])       (set ship)  local one-sided friendships
::    x  /mutuals(/[list])       (set ship)  mutual friendships
::
::    x  /leeches/[ship]         ?  is ship a leeche?
::    x  /targets/[list]/[ship]  ?  is ship a target? list may be ~. for all
::    x  /mutuals/[list]/[ship]  ?  is ship a mutual? list may be ~. for all
::
::      subscription endpoints (local ship only, all %pals-effect marks)
::    /targets   target-effect   effect for every addition/removal
::    /leeches   leeche-effect   effect for every addition/removal
::
/-  *pals
/+  rudder, dbug, verb, default-agent, goon
::
/~  pages  (page:rudder records command)  /app/pals/webui
::
|%
+$  status  ?(%leeche %target %pal)
+$  filter  $~(%all ?(%all %pals %targets %leeches))
++  ui-filter
  |=  [name=term count=@ud value=?]
  :+  name
    :~  lede/(crip "{(trip name)}: {(scow %ud count)}")
        value/f/value
        edit/~
    ==
  ~
++  ui-pals
  |=  [=@p tags=(set term)]
  ^-  goad:goon
  (ui-ship p %pal tags)
::
++  ui-ship
  |=  [patp=@p status=?(%leeche %target %pal) tags=(set term)]
  ^-  goad:goon
  :+  p/patp
    :~  value/p/patp
    ==
  :~  :+  %status
        :~  lede/'Status'
            value/tas/status
        ==
      ~
    ::
      :+  %tags
        :~  lede/'tags'
            hint/~[list/~]
            :-  %act
            :~  [%add 'Add' 'Add a tag to this ship']
            ==
        ==
      %+  turn  ~(tap in tags)
      |=  tag=term
      :+  tag
        :~  value/tas/tag
            :-  %act
            :~  [%del 'delete' 'Delete this tag']
            ==
        ==
      ~
  ==
++  ui-main
  |=  [rec=records =filter tags=(set @tas)]
  ^-  goad:goon
  :+  %pals
    :~  lede/'%pals manager'
        info/'Keep track of your friends! Other applications can use this list to find ships to talk to, pull content from, etc'
    ==
  :~
    :+  %filter
      :~  lede/'filter'
          info/'Selecting these filters will allow you to narrow the list of %pals displayed'
          hint/~[list/~]
      ==
    :~  %+  ui-filter  'all'
        [~(wyt by outgoing.rec) =(%all filter)]
    ::
        %+  ui-filter  'pals'
        [~(wyt in incoming.rec) =(%pals filter)]
    ::
        %+  ui-filter  'targets'
        [~(wyt in (~(dif in ~(key by outgoing.rec)) incoming.rec)) =(%targets filter)]
    ::
        %+  ui-filter  'leeches'
        [~(wyt in (~(dif in incoming.rec) ~(key by outgoing.rec))) =(%leeches filter)]
    ==
  ::
    :+  %labels
      :~  lede/'labels'
          info/'Selecting these labels will allow you to narrow the list of %pals displayed'
          hint/~[list/~]
      ==
    =-  (turn - ui-filter)
    =|  res=(list [tag=@ta num=@ud active=?])
    =-  (turn - |=([@ta @ud] [+<- +<+ |]))
    %~  tap  by
    %+  roll  ~(tap by outgoing.rec)
    |=  [[=ship tags=(set @ta)] acc=(map @ta @ud)]
    %+  roll  ~(tap in tags)
    |=  [tag=@ta acc=_acc]
    (~(put by acc) tag +((~(gut by acc) tag 0)))
  ::
    :+  %list
      :~  lede/'List of pals'
          hint/~[list/~]
      ==
    :-  `goad:goon`[%add ~[value/p/~sampel-palnet add/~] ~]
    =-  (turn - ui-ship)
    %+  turn  ~(tap by outgoing.rec)
    |=  [=ship tags=(set @ta)]
    ^+  [ship *status tags]
    [ship (get-status ship rec) tags]
  ==
++  get-status
  |=  [=ship rec=records]
  ^-  status
  ?+  :-  (~(has in incoming.rec) ship)
      (~(has by outgoing.rec) ship)
    !!
    [%.y %.y]  %pal
    [%.y %.n]  %leeche
    [%.n %.y]  %target
  ==
--
|%
+$  state-0  
  $:  %0
      records
      =filter
      labels=(set @tas)
  ==
::
+$  eyre-id  @ta
+$  card  (wind note gift)
+$  gift  gift:agent:gall
+$  note  note:agent:gall
  :: $%  [%agent [ship %pals] task:agent:gall]
  ::     [%arvo %e %connect [~ %pals ~] term]
  :: ==
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  =^  cards  this
    (on-poke %pals-command !>(`command`[%meet ~paldev ~]))
  =-  [[- cards] this]
  [%pass /eyre/connect %arvo %e %connect [~ /[dap.bowl]] dap.bowl]
::
++  on-save  !>(state)
::
++  on-load
  |=  ole=vase
  ^-  (quip card _this)
  =/  old=state-0  !<(state-0 ole)
  [~ this(state old)]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+  mark  (on-poke:def mark vase)
    ::  %goon-stab
    
      %goon-stab
    =+  !<(=stab:goon vase)
    ?:  =(p.stab /pals/filter/add)
      ?>  ?=(%add -.q.stab)
      ?>  ?=(%iota p.page.q.stab)
      ?>  ?=([%p q=@] q.page.q.stab)
      (on-poke %pals-command !>([%meet `@p`q.q.page.q.stab ~]))
    =/  =(pole knot)  p.stab
    ?:  ?=([%pals %filter filter=?(%all %pals %targets %leeches) ~] pole)
      ?>  ?=(%edit -.q.stab)
      ?>  ?=(%iota p.page.q.stab)
      ?>  ?=([%f q=@] q.page.q.stab)
      =.  filter  ?:(=(& q.q.page.q.stab) filter.pole %all)
      `this
    `this
      
      
    ::  %pals-command: local app control
    ::
      %pals-command
    ?>  =(our src):bowl
    =+  !<(cmd=command vase)
    ?:  (~(has in in.cmd) ~.)
      ~|  [%illegal-empty-list-name in=-.cmd]
      !!
    ?:  =(our.bowl ship.cmd)
      [~ this]
    ::
    =/  known=?  (~(has by outgoing) ship.cmd)
    =;  [yow=? =_outgoing]
      ^-  (quip card _this)
      :_  this(outgoing.state outgoing)
      ?.  yow  ~
      :~  =/  =gesture  ?-(-.cmd %meet [%hey ~], %part [%bye ~])
          =/  =cage     [%pals-gesture !>(gesture)]
          [%pass /[-.gesture] %agent [ship.cmd dap.bowl] %poke cage]
        ::
          =/  =effect   ?-(-.cmd %meet [- ship]:cmd, %part [- ship]:cmd)
          =/  =cage     [%pals-effect !>(effect)]
          [%give %fact [/targets]~ cage]
      ==
    ::
    ?-  -.cmd
        %meet
      :-  !known
      %+  ~(put by outgoing)  ship.cmd
      %-  ~(uni in in.cmd)
      (~(gut by outgoing) ship.cmd ~)
    ::
        %part
      ?:  =(~ in.cmd)
        ::  remove target entirely
        ::
        [known (~(del by outgoing) ship.cmd)]
      ::  remove from specified lists
      ::
      :-  |
      =.  outgoing
        =/  liz=(list @ta)  ~(tap in in.cmd)
        |-  ^+  outgoing
        ?~  liz  outgoing
        $(liz t.liz, outgoing (~(del ju outgoing) ship.cmd i.liz))
      ::NOTE  we could account for this above, but +del:ju is just easier there
      =?  outgoing  !(~(has by outgoing) ship.cmd)
        (~(put by outgoing) ship.cmd ~)
      outgoing
    ==
  ::
    ::  %pals-gesture: foreign %pals signals
    ::
      %pals-gesture
    ?<  =(our src):bowl
    =*  ship  src.bowl
    =+  !<(=gesture vase)
    =/  [yow=? =_incoming]
      =*  has  (~(has in incoming) ship)
      ?-  -.gesture
        %hey  :-  !has  (~(put in incoming) ship)
        %bye  :-   has  (~(del in incoming) ship)
      ==
    :_  this(incoming.state incoming)
    ^-  (list card)
    ?.  yow  ~
    :*  =/  =effect  ?-(-.gesture %hey [%near ship], %bye [%away ship])
        =/  =cage    [%pals-effect !>(effect)]
        [%give %fact [/leeches]~ cage]
      ::
        ?.  .^(? %gu /(scot %p our.bowl)/hark/(scot %da now.bowl)/$)  ~
        =/  body
          =-  [ship+ship - ~]
          ?-  -.gesture
            %hey  ' added you as a pal.'
            %bye  ' no longer considers you a pal.'
          ==
        =/  id      (end 7 (shas %pals-notification eny.bowl))
        =/  rope    [~ ~ q.byk.bowl /(scot %p ship)/[-.gesture]]
        =/  action  [%add-yarn & & id rope now.bowl body /pals ~]
        =/  =cage   [%hark-action !>(action)]
        [%pass /hark %agent [our.bowl %hark] %poke cage]~
    ==
  ::
    ::  %handle-http-request: incoming from eyre
    ::
      %handle-http-request
    =;  out=(quip card _+<.state)
      [-.out this(+<.state +.out)]
    %.  [bowl !<(order:rudder vase) +<.state]
    %-  (steer:rudder _+<.state command)
    :^    pages
        (point:rudder /[dap.bowl] & ~(key by pages))
      (fours:rudder +<.state)
    |=  cmd=command
    ^-  $@  brief:rudder
        [brief:rudder (list card) _+<.state]
    =^  caz  this
      (on-poke %pals-command !>(cmd))
    ['Processed succesfully.' caz +<.state]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  =(our.bowl src.bowl)
  ?+  path  (on-watch:def path)
    [%goon ~]           `this
    [%http-response *]  [~ this]
  ::
      [%targets ~]
    :_  this
    %+  turn  ~(tap in ~(key by outgoing))
    |=(=@p [%give %fact ~ %pals-effect !>(`effect`[%meet p])])
  ::
      [%leeches ~]
    :_  this
    %+  turn  ~(tap in incoming)
    |=(=@p [%give %fact ~ %pals-effect !>(`effect`[%near p])])
  ::
    ::TODO  consider adding a subscription endpoint that includes tags?
    ::      shouldn't become too legible to applications though...
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  wire  ~&([dap.bowl %strange-wire wire] [~ this])
      [%hark ~]
    ?.  ?=(%poke-ack -.sign)  (on-agent:def wire sign)
    ?~  p.sign  [~ this]
    ((slog 'pals: failed to notify' u.p.sign) [~ this])
  ::
      [%bye ~]  [~ this]  ::TODO  also retry if nack?
      [%hey ~]
    ::  for %pals-gesture pokes, record the result
    ::TODO  should we slowly retry for nacks?
    ::
    =-  [~ this(receipts -)]
    ?+  -.sign  ~|([%unexpected-agent-sign wire -.sign] !!)
      %poke-ack  (~(put by receipts) src.bowl ?=(~ p.sign))
    ==
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?>  =(our src):bowl
  |^  ?+  path  [~ ~]
        [%y ~]                 (arc %leeches %targets %mutuals ~)
        [%y %leeches ~]        (arc ~)
        [%y %targets ~]        (arc (las targets))
        [%y %mutuals ~]        (arc (las mutuals))
        [%x %leeches ~]        (alp leeches)
        [%x %leeches @ ~]      (ask (bind (slaw %p i.t.t.path) (sin leeches)))
        [%x %targets ~]        (alp targets)
        [%x %targets ~ ~]      [~ ~]
        [%x %targets @ta ~]    (alp (lap targets i.t.t.path))
        [%x %targets @ta @ ~]  (ask (bind (wat t.t.path) (hal targets)))
        [%x %mutuals ~]        (alp mutuals)
        [%x %mutuals ~ ~]      [~ ~]
        [%x %mutuals @ta ~]    (alp (lap mutuals i.t.t.path))
        [%x %mutuals @ta @ ~]  (ask (bind (wat t.t.path) (hal mutuals)))
      ::
          [%x %json ~]  ::NOTE  dumb hack, subject to change
        =;  =json  ``json+!>(json)
        =,  enjs:format
        %-  pairs
        :~  :-  'outgoing'
            %-  pairs
            %+  turn  ~(tap by outgoing)
            |=  [=^ship lists=(set @ta)]
            :-  (rsh 3 (scot %p ship))
            %-  pairs
            :~  'lists'^a+(turn ~(tap in lists) (lead %s))
                'ack'^(fall (bind (~(get by receipts) ship) (lead %b)) ~)
            ==
          ::
            :-  'incoming'
            %-  pairs
            %+  turn  ~(tap in incoming)
            |=(=^^ship [(rsh 3 (scot %p ship)) b+&])
        ==
      ::
          [%x %goon *]
        =-  ``noun+!>(-)
        ^-  goad:goon
        (ui-main +.state)
      ==
  ::  scry results
  ++  arc  |=  l=(list @ta)  ``noun+!>(`arch`~^(malt (turn l (late ~))))
  ++  alp  |=  s=(set @p)    ``noun+!>(s)
  ++  alf  |=  f=?           ``noun+!>(f)
  ++  ask  |=  u=(unit ?)  ?^(u (alf u.u) [~ ~])
  ::  data wrestling
  ++  wat  |=([l=@ta p=@ta ~] ?~(p=(slaw %p p) ~ (some [l u.p])))
  ++  nab  ~(got by outgoing)
  ++  las  |=(s=(set @p) (zing (turn (sit s) |=(=@p (sit (nab p))))))
  ++  lap  |=([s=(set @p) l=@ta] (ski s |=(=@p ((sin (nab p)) l))))
  ++  hal  |=(s=(set @p) |=([l=@ta =@p] ((sin ?~(l s (lap s l))) p)))
  ::  set shorthands
  ++  sin  |*(s=(set) ~(has in s))
  ++  sit  |*(s=(set) ~(tap in s))
  ++  ski  |*([s=(set) f=$-(* ?)] (sy (skim (sit s) f)))
  ::  pals
  ++  leeches  incoming
  ++  targets  ~(key by outgoing)
  ++  mutuals  (~(int in targets) leeches)
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+  sign-arvo  (on-arvo:def wire sign-arvo)
      [%eyre %bound *]
    ~?  !accepted.sign-arvo
      [dap.bowl 'eyre bind rejected!' binding.sign-arvo]
    [~ this]
  ==
::
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--

