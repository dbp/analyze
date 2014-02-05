<apply template="base">

  <bind tag="site-name"><name/></bind>

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
      <p>
        <error-summary/>
        (last happened at <example><time/></example>)
      </p>
    </not-resolved>
  </errors>

  <h4>Top URLs Today <span class="right">ordered by log(hits)*log(avg)</span></h4>
  <table>
    <tr>
      <th>url</th>
      <th>method</th>
      <th>hits</th>
      <th>max</th>
      <th>min</th>
      <th>avg</th>
      <th>var</th>
    </tr>
    <visits>
      <tr>
        <td><url/></td>
        <td><method/></td>
        <td><hits/></td>
        <td><max/>ms</td>
        <td><min/>ms</td>
        <td><avg/>ms</td>
        <td><var/>ms</td>
      </tr>
    </visits>
  </table>


  <h4>Days</h4>
  <p>
    <days>
      <a href="/site/${id}/day/${formatted}"><formatted/></a> |
    </days>
  </p>

  <h4>Resolved Errors</h4>
  <errors>
    <is-resolved>
      <p>
        <error-summary/>
        (resolved at <resolved/>)
      </p>
    </is-resolved>
  </errors>


  <h4>Tokens</h4>
  <div class="p">
    <a href="/site/${id}/token/new">Create New Token</a><br/>
    <tokens>
      <not-invalidated>
        <div class="p">
          <token/> (<created/>)<br/>
        </div>
      </not-invalidated>
    </tokens>
  </div>
</apply>
