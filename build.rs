use lettre::{message::header::ContentType, transport::smtp::{authentication::Credentials, client::SmtpConnection}, Message};

fn main() {
    lalrpop::process_root().unwrap();
    
    // let email = Message::builder()
    //     .from("crane cranesweetie@gmail.com".parse().unwrap())
    //     .to("crane 15352461680@126.com".parse().unwrap())
    //     .subject("Hello")
    //     .header(ContentType::TEXT_PLAIN)
    //     .body(String::from("Hello, world!"))
    //     .unwrap();
    // let creds = Credentials::new(username,password);
}
  