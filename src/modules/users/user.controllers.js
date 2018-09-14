import User from './user.model';

const client = require('twilio')('ACdcf9039846ddd1542aee439dff3170c2', '13519f53eed795553b4901a55c1ebee0');

export async function signup(req, res) {
  const code = Math.floor(Math.random() * 9000) + 1000;
  try {
    const resp = await client.messages
      .create({
        body: `this is your code: ${code}`,
        from: '+18509905322',
        to: '+905369269145',
      });
    console.log('====================================');
    console.log(resp);
    console.log('====================================');
  } catch (e) {
    return res.status(500).json(e);
  }
}
