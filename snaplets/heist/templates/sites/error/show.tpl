<apply template="base">
  <h4>Error ID#<id/> <span class="right"><a href="/site/${site-id}">back</a></span></h4>

  <p>Message: <message/></p>
<is-resolved>
  <p>Resolved at <resolved/> (<a href="/site/${site-id}/error/${id}/resolve">unresolve</a>)</p>
</is-resolved>
<not-resolved>
  <p>Not Resolved (<a href="/site/${site-id}/error/${id}/resolve">resolve</a>)</p>
</not-resolved>
<p>Created at <created/></p>

<div class="p">
  <has-issue-id>
    <bindStrict tag="lnk-iss"><site><issue-link id="${issue-id}"/></site></bindStrict>
    Issue: <a href="${lnk-iss}"><lnk-iss/></a>
    <form style="display: inline" method="post" action="/site/${site-id}/error/${id}/issue">
      <input type="submit" value="clear"/>
    </form>
  </has-issue-id>
  <no-issue-id>
    Issue: <form style="display: inline" method="post" action="/site/${site-id}/error/${id}/issue">
      <input type="text" name="issue"/>
      <input type="submit" value="set"/>
    </form>
  </no-issue-id>
</div>

<br/>
<h4>
Examples:
</h4>
<br/>

<table>
  <tr>
    <th>url</th>
    <th>time</th>
    <th>user</th>
  </tr>
  <examples>
    <tr>
      <td><url/></td>
      <td><time/></td>
      <td>
        <has-user-id>
          <bindStrict tag="lnk-us"><site><user-link id="${user-id}"/></site></bindStrict>
          <a href="${lnk-us}"><lnk-us/></a>
        </has-user-id>
      </td>
    </tr>
  </examples>
</table>
</apply>
