<apply template="base">
<p>Message: <message/></p>
<is-resolved>
  <p>Resolved at <resolved/></p>
</is-resolved>
<not-resolved>
  <p>Not Resolved</p>
</not-resolved>
<p>Created at <created/></p>

<has-issue-id>
  <bindStrict tag="lnk-iss"><site><issue-link id="${issue-id}"/></site></bindStrict>
  Issue: <a href="${lnk-iss}"><lnk-iss/></a>
</has-issue-id>
<br/>
Examples:
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
