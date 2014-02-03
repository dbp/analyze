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
  <bind tag="lnk"><site><issue-link id="${issue-id}"/></site></bind>
  Issue: <a href="${lnk}"><lnk/></a>
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
          <bind tag="lnk"><site><user-link id="${user-id}"/></site></bind>
          <a href="${lnk}"><lnk/></a>
        </has-user-id>
      </td>
    </tr>
  </examples>
</table>
</apply>
