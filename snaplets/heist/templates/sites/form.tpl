
<dfForm method="post">
  <table id="info">
    <tr><td colspan=2><message/></td></tr>
    <tr>
      <td><dfLabel ref="name">Name:</dfLabel></td>
      <td><dfInputText ref="name" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="name">
      <tr>
        <td></td><td><dfErrorList ref="name" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td><dfLabel ref="url">URL:</dfLabel></td>
      <td><dfInputText ref="url" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="url">
      <tr>
        <td></td><td><dfErrorList ref="url" /></td>
      </tr>
    </dfIfChildErrors>

    <dfSubView ref="start_date">
      <apply template="date_form"></apply>
    </dfSubView>

    <tr>
      <td><dfLabel ref="user_link_pattern">User Link Pattern (id is *):</dfLabel></td>
      <td><dfInputText ref="user_link_pattern" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="user_link_pattern">
      <tr>
        <td></td><td><dfErrorList ref="user_link_pattern" /></td>
      </tr>
    </dfIfChildErrors>

    <tr>
      <td><dfLabel ref="issue_link_pattern">Issue Link Pattern (id is *):</dfLabel></td>
      <td><dfInputText ref="issue_link_pattern" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="issue_link_pattern">
      <tr>
        <td></td><td><dfErrorList ref="issue_link_pattern" /></td>
      </tr>
    </dfIfChildErrors>


    <tr>
      <td></td>
      <td><dfInputSubmit/></td>
    </tr>
  </table>
</dfForm>
