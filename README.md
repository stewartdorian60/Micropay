# 💰 Micropay - Peer-to-Peer Microloans

A decentralized platform for community-funded microloans with automated smart repayments built on the Stacks blockchain.

## 🌟 Features

- **🤝 Peer-to-Peer Lending**: Direct lending between community members
- **💸 Microloan Support**: Small loans for individuals and small businesses
- **📊 Community Funding**: Multiple funders can contribute to a single loan
- **⚡ Smart Repayments**: Automated repayment scheduling and distribution
- **🏆 Reputation System**: Track lending and borrowing history
- **🔒 Collateral Management**: Automated handling of overdue loans

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Run Clarinet commands to deploy and test

```bash
clarinet check
clarinet test
clarinet deploy
```

## 📖 Usage

### For Borrowers 👤

#### Request a Loan
```clarity
(contract-call? .Micropay request-loan u1000000 u500 u1008 "Equipment for small business")
```
- `amount`: Loan amount in microSTX
- `interest-rate`: Interest rate in basis points (500 = 5%)
- `duration-blocks`: Loan duration in blocks
- `description`: Purpose of the loan

#### Make Repayments
```clarity
(contract-call? .Micropay make-repayment u1 u250000)
```
- `loan-id`: ID of the loan
- `payment-amount`: Amount to repay in microSTX

### For Lenders 💼

#### Fund a Loan
```clarity
(contract-call? .Micropay fund-loan u1 u500000)
```
- `loan-id`: ID of the loan to fund
- `fund-amount`: Amount to contribute in microSTX

#### Check Loan Details
```clarity
(contract-call? .Micropay get-loan u1)
```

### For Everyone 🌍

#### View User Statistics
```clarity
(contract-call? .Micropay get-user-stats 'SP1234...)
```

#### Get Total Loans Count
```clarity
(contract-call? .Micropay get-total-loans)
```

## 🏗️ Contract Structure

### Core Functions

- **`request-loan`**: Create a new loan request
- **`fund-loan`**: Contribute funds to a loan
- **`make-repayment`**: Make loan repayments
- **`claim-overdue-collateral`**: Handle defaulted loans

### Read-Only Functions

- **`get-loan`**: Retrieve loan information
- **`get-user-stats`**: Get user lending/borrowing statistics
- **`calculate-loan-total`**: Calculate total repayment amount
- **`get-repayment-info`**: Get repayment schedule details
