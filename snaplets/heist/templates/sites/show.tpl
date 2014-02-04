<apply template="base">

  <h3><name/></h3>

  <ignore>dynamic scope - beware!</ignore>
  <bind tag="error-summary">
    <a href="/site/${site-id}/error/${id}">view</a>
    <has-issue-id>
      <bindStrict tag="iss-lnk"><issue-link id="${issue-id}"/></bindStrict>
      <a href="${iss-lnk}">issue</a>
    </has-issue-id>
    <message/>
  </bind>

  <h4>Unresolved Errors</h4>
  <rebind old="id" new="site-id"/>
  <errors>
    <not-resolved>
      <error-summary/>
      (last happened at <example><time/></example>)<br/>
    </not-resolved>
  </errors>

  <h4>Days</h4>
  <days>
    <a href="/site/${id}/day/${formatted}"><formatted/></a> |
  </days>

  <h4>Resolved Errors</h4>
  <errors>
    <is-resolved>
      <error-summary/>
      (resolved at <resolved/>)<br/>
    </is-resolved>
  </errors>


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
