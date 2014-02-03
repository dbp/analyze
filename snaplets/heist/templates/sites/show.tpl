<apply template="base">

  <h3><name/></h3>

  <h4>Errors</h4>
  <rebind old="id" new="site-id"/>
  <errors>
    <message/> <a href="/site/${site-id}/error/${id}">view</a><br/>
  </errors>

  <h4>Days</h4>
  <days>
    <a href="/site/${id}/day/${formatted}"><formatted/></a> |
  </days>

  <h4>Tokens</h4>
  <a href="/site/${id}/token/new">Create New Token</a><br/>
  <tokens>
    <not-invalidated>
      <br/>
      <div>
        <token/> (<created/>)<br/>
        <ul>
          <li>/submit/visit?url=FOO&render=NNN&token=<token/></li>
          <li>/submit/error?url=FOO&message=BAR&uid=N&token=<token/></li>
        </ul>
      </div>
    </not-invalidated>
  </tokens>
</apply>
